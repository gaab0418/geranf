#include 'protheus.ch'
#include 'parmtype.ch'
#Include "TOTVS.ch"
#Include "RESTFUL.ch"
#Include "Colors.ch"
#Include "RPTDef.ch"
#Include "FWPrintSetup.ch"
#Include "TopConn.ch"

/*/{Protheus.doc} WsGRNFNF
Ws rest para o GRNF - Geraï¿½ï¿½o de NF na aprovaï¿½ï¿½o do SE
@author  gabriel.souza
@since   30/05/2025
@version 1.0
@type function
/*/
// #################################################################################################################################
WSRESTFUL WsGRNFNF DESCRIPTION 'WebService Geraï¿½ï¿½o de NF na aprovaï¿½ï¿½o do SE'

	//Mï¿½todos
	WSMETHOD GET    HOME        DESCRIPTION "Retorna versï¿½o da API"     PATH ""             WSSYNTAX "/GET/"               //PRODUCES APPLICATION_JSON

	WSMETHOD POST   DANFE       DESCRIPTION "Download do Danfe"         PATH "/DANFE"      WSSYNTAX "WSGRNFNF/DANFE/"    //PRODUCES APPLICATION_JSON
	WSMETHOD POST   RETORNA     DESCRIPTION "Retorno e Devoluï¿½ï¿½o"       PATH "/RETORNO"    WSSYNTAX "WSGRNFNF/RETORNO/"  //PRODUCES APPLICATION_JSON

END WSRESTFUL
// #################################################################################################################################
WsMethod GET HOME WsService WsGRNFNF

	Local jResponse := JsonObject():New()

	jResponse       := U_KROWSHOME()
	Self:SetContentType('application/json')
	Self:SetResponse(jResponse:toJSON())

Return .T.
// #################################################################################################################################
WsMethod POST RETORNA WsService WsGRNFNF

	Local jResponse             := JsonObject():New()
	Local oJson                 := JsonObject():New()
	Local oFile                 := Nil
	Local lOk                   := .T.
	Local cBody                 := ""
	Local cMsgErro              := ""
	Local cFile                 := ""
	Local cErroTxt              := "Erro"
	Local aRet                  := {}
	Local aCamposObrt           := {;
		{"pedido",              .T.}}

	Private nTamNota         := 0
	Private nTamSerie        := 0
	Private cPasta1          := ""
	Private cPasta2          := ""
	Private xFile            := ""
	Private cIdent           := ""
	Private PixelX           := 0
	Private PixelY           := 0
	Private nConsNeg         := 0
	Private nConsTex         := 0
	Private nColAux          := 0
	Private oRetNF           := Nil

	jResponse                := U_KROWSHOME() //cabeï¿½alho padrï¿½o para rests
	jResponse['pedido']      := ""
	jResponse['nf']          := ""
	jResponse['serie']       := ""
	jResponse['chave']       := ""
	jResponse['arquivo_pdf'] := ""
	jResponse['error']       := ""
	jResponse['arquivo']     := ""
	cBody                    := Self:GetContent()
	cMsgErro                 := oJson:FromJson( cBody )

	If Empty(cMsgErro)  // validacoes de campos preenchidos
		aRet        := hasErrorJson(oJson, aCamposObrt)
		lOk         := aRet[1]
		cMsgErro    := aRet[2]

	Else
		lOk         := .F.
		cMsgErro    += " - [Erro] - Ao Parsear Json"

	EndIf

	If lOk
		nTamNota	    := TamSX3('F2_DOC'	)[1]
		nTamSerie	    := TamSX3('F2_SERIE')[1]
		cPasta1		    := SuperGetMV("KR_GRNF036", .F., "GRNF03\retorno_nf\danfe")
		cPasta2		    := SuperGetMV("KR_GRNF037", .F., "GRNF03\retorno_nf\print")

		aRet := fRetornoNF(oJson)

		jResponse['pedido']         := aRet[2]["pedido"]
		jResponse['nf']             := aRet[2]["nf"]
		jResponse['serie']          := aRet[2]["serie"]
		jResponse['chave']          := aRet[2]["chave"]
		jResponse['arquivo_pdf']    := aRet[2]["arquivo_pdf"]
		jResponse['error']          := aRet[2]["error"]

		If aRet[1]
			cArq    := aRet[2]["arquivo_pdf"]
			oFile   := FwFileReader():New(cArq)

			If (oFile:Open()) .AND. File(cArq)
				cFile       := oFile:FullRead()
				cFile64     := Encode64( cFile )
				jResponse['arquivo']    := cFile64

				Self:SetResponse(jResponse)
				oFile:Close()

			Else
				lOk                     := .F.
				jResponse['status']     := cErroTxt
				jResponse['error']      := "Nao foi possivel carregar o arquivo [" + cArq + "]"

				Self:SetStatus( 200 )
				Self:SetResponse( jResponse )
			Endif
		Else
			lOk := .F.
			jResponse['status'] := cErroTxt
			Self:SetStatus( 200 )
			Self:SetResponse( jResponse )
		EndIf
	Else
		jResponse['error']      := cMsgErro
		jResponse['status']     := cErroTxt
		Self:SetStatus( 400 )
		Self:SetResponse( jResponse )
	EndIf

Return
// #################################################################################################################################
WsMethod POST DANFE WsService WsGRNFNF

	Local jResponse             := JsonObject():New()
	Local oJson                 := Nil
	Local oFile                 := Nil
	Local lOk                   := .T.
	Local cBody                 := ""
	Local cMsgErro              := ""
	Local cFile                 := ""
	Local cErroTxt              := "Erro"
	Local aRet                  := {}
	Local aCamposObrt           := {;
		{"codigo_fornecedor",       .T.},;
		{"loja",                    .T.},;
		{"codigo_transportadora",   .T.},;
		{"numero_nf_devolvida",     .F.},;
		{"condicao_pagamento",      .T.},;
		{"numero_solicitacao",      .T.},;
		{"solicitante",             .T.},;
		{"tipo_volume",             .T.},;
		{"quantidade_volume",       .T.},;
		{"peso_kg",                 .T.},;
		{"tipo_saida",              .T.},;
		{"cod_rv",                  .F.},;
		{"observacao",              .F.},;
		{"tipo_pedido",             .T.},;
		{"produtos",                .T.},;
		{;
		{"codigo_produto",          .T.},;
		{"quantidade_produto",      .T.},;
		{"valor_produto",           .T.},;
		{"almoxarifado",            .T.},;
		{"tes",                     .T.};
		}}

	Private nTamNota	    := 0
	Private nTamSerie	    := 0
	Private cPasta1		    := ""
	Private cPasta2		    := ""
	Private xFile		    := ""
	Private cIdent		    := ""
	Private PixelX		    := 0
	Private PixelY		    := 0
	Private nConsNeg	    := 0
	Private nConsTex	    := 0
	Private nColAux		    := 0
	Private oRetNF          := Nil

	oJSon                   := JsonObject():New()
	cBody                   := Self:GetContent()
	cMsgErro                := oJson:FromJson( cBody )

	// validacoes de erro
	If Empty(cMsgErro)
		// validacoes de campos preenchidos
		aRet        := hasErrorJson(oJson, aCamposObrt)
		lOk         := aRet[1]
		cMsgErro    := aRet[2]

	Else
		lOk         := .F.
		cMsgErro    += " - [Erro] - Ao Parsear Json"

	Endif

	nTamNota	    := TamSX3('F2_DOC'	)[1]
	nTamSerie	    := TamSX3('F2_SERIE')[1]
	cPasta1		    := SuperGetMV("KR_GRNF036", .F., "GRNF03\gera_nf\danfe")
	cPasta2		    := SuperGetMV("KR_GRNF037", .F., "GRNF03\gera_nf\danfe_tmp")

	jResponse                := U_KROWSHOME() //cabeï¿½alho padrï¿½o para rests
	jResponse['pedido']      := ""
	jResponse['nf']          := ""
	jResponse['serie']       := ""
	jResponse['chave']       := ""
	jResponse['arquivo_pdf'] := ""
	jResponse['error']       := ""
	jResponse['arquivo']     := ""

	If lOk

		aRet := fGeraNF(oJson)
		jResponse['pedido']      := aRet[2]['pedido']
		jResponse['nf']          := aRet[2]['nf']
		jResponse['serie']       := aRet[2]['serie']
		jResponse['chave']       := aRet[2]['chave']
		jResponse['arquivo_pdf'] := aRet[2]['arquivo_pdf']
		jResponse['error']       := aRet[2]['error']

		If aRet[1] == .T.
			cArq    := aRet[2]["arquivo_pdf"]
			oFile   := FwFileReader():New(cArq)

			If (oFile:Open()) .AND. File(cArq)
				cFile       := oFile:FullRead()
				cFile64     := Encode64( cFile )
				jResponse['arquivo']    := cFile64

				Self:SetResponse(jResponse)
				oFile:Close()

			Else
				lOk                     := .F.
				cArq                    := "Nao foi possivel carregar o arquivo"
				jResponse['status']     := cErroTxt
				jResponse['error']      := cArq

				Self:SetStatus( 200 )
				Self:SetResponse( jResponse )

			Endif
		Else
			lOk                     := .F.
			cMsgErro                := aRet[2]['error']
			jResponse['status']     := cErroTxt

			Self:SetStatus( 200 )
			Self:SetResponse( jResponse )

		Endif
	Endif

	If !lOk .AND. Empty(jResponse['error'])
		jResponse['error']      := cMsgErro
		jResponse['status']     := cErroTxt
		Self:SetStatus( 400 )
		Self:SetResponse( jResponse )

	Endif

Return
// #################################################################################################################################
Static Function hasErrorJson(oJson, aCamposObrt)

	Local nX                    := 1
	Local nY                    := 1
	Local lOk                   := .T.
	Local lEhObrig              := .F.
	Local cMsgErro              := ""
	Local cCampo                := ""
	Local jAux                  := JsonObject():New()

	If !Empty(aCamposObrt)
		For nX := 1 to Len(aCamposObrt)
			cCampo      := aCamposObrt[nX][1]
			lEhObrig    := aCamposObrt[nX][2]

			If lOk == .F.
				Exit
			Endif

			If ValType( cCampo ) <> "A"
				If !oJson:HasProperty(cCampo) .OR. Iif(lEhObrig, Empty(oJson:GetJsonText(cCampo)), .F.)
					cMsgErro := "[Erro] - Campo nao existe ou esta vazio! " + cCampo
					lOk      := .F.
					Exit

				Endif
			Else
				For nY := 1 to Len(aCamposObrt[nX])
					cCampo      := aCamposObrt[nX][nY][1]
					lEhObrig    := aCamposObrt[nX][nY][2]
					jAux        := oJson[aCamposObrt[nX-1][1]][1] // coloca a posicao no titulo do campo

					If !jAux:HasProperty(cCampo) .OR. Iif(lEhObrig, Empty(jAux[cCampo]), .F.)
						cMsgErro := "[Erro] - Campo dentro de "+ cCampo +" nao existe " + Iif(lEhObrig, "ou esta vazio!: ", ": ") + cCampo
						lOk      := .F.
						Exit

					Endif
				Next
			Endif
		Next
	Endif

Return {lOk, cMsgErro}
// #################################################################################################################################
Static Function fGeraNF(oJson)

	Local jRetorno          := JsonObject():New()
	Local aRet              := {}
	Local aAux              := {}
	Local lOk               := .T.
	Local _cPedido          := ""
	Local _cNota            := ""
	Local _cSerie           := ""
	Local _cChave           := ""

	jRetorno["pedido"]      := ""
	jRetorno["nf"]          := ""
	jRetorno["serie"]       := ""
	jRetorno["chave"]       := ""
	jRetorno["arquivo_pdf"] := ""
	jRetorno["error"]       := ""

	aRet := fGeraPedido(oJson)

	If lOk .AND. aRet[1] == .T.

		_cPedido            := aRet[2]
		jRetorno["pedido"]  := _cPedido
		aRet                := {}
		Aadd(aRet, _cPedido)
		DbSelectArea("SC6")
		SC6->(DbSetOrder(1))
		If SC6->(MsSeek( xFilial("SC6") + _cPedido ))
			If SC6->C6_QTDVEN > 0
				U_KRO1041( " ROBO GRNF - [GRNF03 - GERA NF] := Pedido Gerado com sucesso! [" + _cPedido + "]")
				lOk := fLiberaPedido(_cPedido)

				DbSelectArea("SC9")
				SC9->(DbSetOrder(1))
				If lOk
					If SC9->(MsSeek( xFilial("SC9") + _cPedido ))
						U_KRO1041( " ROBO GRNF - [GRNF03 - GERA NF] := Pedido Liberado com sucesso! [" + _cPedido + "]")

						aAux := fGeraNFe(_cPedido)

						_cSerie             := aAux[2]["serie"]
						_cNota              := aAux[2]["nf"]
						_cChave             := aAux[2]["chave"]
						jRetorno["serie"]   := _cSerie
						jRetorno["nf"]      := _cNota
						jRetorno["chave"]   := _cChave
						If aAux[1] == .T.

							aAux := fPrintNF(_cNota, _cSerie, _cChave)
							If aAux[1] == .T.
								jRetorno['arquivo_pdf'] := aAux[2]
								U_KRO1041( " ROBO GRNF - [GRNF03 - GERA NF] := Arquivo gerado / Arquivo: [" + aAux[2] + "]")

							Else
								lOk := .F.
								jRetorno["error"] := "Erro ao gerar arquivo Danfe.pdf"

							Endif
						Else
							lOk := .F.
							jRetorno["error"] := aAux[2, 'error']
						Endif
					Else
						lOk := .F.
						jRetorno["error"] := "Nao foi possivel localizar o documento no banco de dados! Pedido: " + _cPedido
					Endif
				Else
					jRetorno["error"] := "Nao foi possivel liberar o pedido! Pedido: " + _cPedido
				Endif
			Else
				lOk := .F.
				jRetorno["error"] := "Pedido sem quantidade a liberar! Pedido: " + _cPedido
			Endif
		Else
			lOk := .F.
			jRetorno["error"] := "Pedido nao encontrado no banco de dados! Pedido: " + _cPedido
		Endif
	Else
		lOk := .F.
		jRetorno["error"] := "Erro ao criar pedido: " + aRet[2]
	Endif

	SC6->(DbCloseArea())
	SC9->(DbCloseArea())

Return {lOk, jRetorno}
// #################################################################################################################################
Static Function fRetornoNF(oJson)

	Local jRetorno          := JsonObject():New()
	Local aRet              := {}
	Local aAux              := {}
	Local lOk               := .T.
	Local _cPedido          := ""
	Local _cNota            := ""
	Local _cSerie           := ""
	Local _cChave           := ""

	jRetorno["pedido"]      := ""
	jRetorno["nf"]          := ""
	jRetorno["serie"]       := ""
	jRetorno["chave"]       := ""
	jRetorno["arquivo_pdf"] := ""
	jRetorno["error"]       := ""


	_cPedido            := oJson['pedido']
	jRetorno["pedido"]  := _cPedido
	aRet                := {}
	Aadd(aRet, _cPedido)

	SC6->(DbSetOrder(1))
	If SC6->(MsSeek( xFilial("SC6") + _cPedido ))
		If SC6->C6_QTDVEN > 0

			U_KRO1041( " ROBO GRNF - [GRNF03 - RETORNO] := Pedido EXISTE! [" + _cPedido + "]")
			lOk := fLiberaPedido(_cPedido)

			SC9->(DbSetOrder(1))
			If lOk
				If SC9->(MsSeek( xFilial("SC9") + _cPedido ))

					aAux := fGeraNFe(_cPedido)

					_cSerie             := aAux[2]["serie"]
					_cNota              := aAux[2]["nf"]
					_cChave             := aAux[2]["chave"]
					jRetorno["serie"]   := _cSerie
					jRetorno["nf"]      := _cNota
					jRetorno["chave"]   := _cChave
					If aAux[1] == .T.

						aAux := fPrintNF(_cNota, _cSerie, _cChave)
						If aAux[1] == .T.
							jRetorno['arquivo_pdf'] := aAux[2]
							U_KRO1041( " ROBO GRNF - [GRNF03 - RETORNO] := Arquivo gerado / Arquivo: [" + aAux[2] + "]")

						Else
							lOk := .F.
							jRetorno["error"] := "Erro ao gerar arquivo Danfe.pdf"

						Endif
					Else
						lOk := .F.
						jRetorno["error"] := aAux[2, 'error']
					Endif
				Else
					lOk := .F.
					jRetorno["error"] := "Nao foi possivel localizar o documento no banco de dados! Pedido: " + _cPedido
				Endif
			Else
				jRetorno["error"] := "Nao foi possivel liberar o pedido! Pedido: " + _cPedido
			Endif
			SC9->(DbCloseArea())
		Else
			lOk := .F.
			jRetorno["error"] := "Pedido sem quantidade a liberar! Pedido: " + _cPedido
		Endif
	Else
		lOk := .F.
		jRetorno["error"] := "Pedido nao encontrado no banco de dados! Pedido: " + _cPedido
	Endif


	SC6->(DbCloseArea())

Return {lOk, jRetorno}
// #################################################################################################################################
Static Function fLiberaPedido( _cPed )

	Local lRet          := .F.
	Local lLiber	    := .F.
	Local lTransf	    := .F.

	DbSelectArea("SC6")
	SC6->(DbSetOrder(1))
	If SC6->(MsSeek( xFilial("SC6") + _cPed ))
		While !(SC6->(Eof())) .AND. SC6->C6_NUM == _cPed
			MaLibDoFat( SC6->(Recno()), SC6->C6_QTDVEN, .T., .T., .F., .F., lLiber, lTransf )
			lRet := .T.
			SC6->(DbSkip())

		EndDo
	Endif
	SC6->(DbCloseArea())

Return lRet
// #################################################################################################################################
Static Function fPrintNF(_cDoc, _cSerie, _cChave)

	Local lOk           := .T.
	Local xRet          := {}

	U_KRO1041(" ROBO GRNF - [ Print PDF ] - Iniciando processo...")
	If !ExistDir(cPasta2)
		U_KRO1041(" ROBO GRNF - [ Print PDF ] - Pasta nï¿½o existe... [" + cPasta2 + "]")
		FwMakeDir(cPasta2)
		U_KRO1041(" ROBO GRNF - [ Print PDF ] - Pasta criada! [" + cPasta2 + "]")
	Endif

	DbSelectArea("SF2")
	SF2->(DbSetOrder(1))

	If SF2->(MsSeek( xFilial("SF2") + _cDoc + _cSerie ))

		If !Empty( _cChave )
			If !File( _cChave + '.pdf')
				xRet    := fGerPDFNF(SF2->F2_FILIAL, _cDoc, _cSerie, _cChave)
				U_KRO1041(" ROBO GRNF - [ Print PDF ] - Arquivo gerado! [" + xRet + "]")

			Else
				xRet    := cPasta1 + _cChave+'.pdf'
				U_KRO1041(" ROBO GRNF - [ Print PDF ] - Arquivo jï¿½ existe! [" + xRet + "]")
				lOk     := .F.
			Endif
		Else
			xRet    := "A NF esta sem chave!"
			U_KRO1041(" ROBO GRNF - [ Print PDF ] - A NF esta " + xRet)
			lOk     := .F.
		Endif
	Else
		xRet    := "O registro nao foi achado no banco de dados!"
		U_KRO1041(" ROBO GRNF - [ Print PDF ] - " + xRet)
		lOk     := .F.
	Endif
	SF2->(DbCloseArea())

Return {lOk, xRet}
// #################################################################################################################################
Static Function fGerPDFNF(xFilial, cNota, cSerie, zChave)

	Local oDanfe	:= Nil

	Default cNota	:= ""
	Default cSerie	:= ""

	U_KRO1041(" ROBO GRNF - [ Gerar Danfe XML ] - Iniciando processo...")
	If !ExistDir(cPasta1)
		U_KRO1041(" ROBO GRNF - [ Gerar Danfe XML ] - Pasta nï¿½o existe... [" + cPasta1 + "]")
		FwMakeDir(cPasta1)
		U_KRO1041(" ROBO GRNF - [ Gerar Danfe XML ] - Pasta criada! [" + cPasta1 + "]")

	Endif

	If !Empty(cNota)

		cIdent  := RetIdEnti()
		If SubStr(cPasta1, Len(cPasta1), 1) != "\"
			cPasta1 += "\"
		Endif

		cArquivo := zChave
		xFile	 := cPasta1 + cArquivo + '.pdf'
		Pergunte("NFSIGW",.F.)
		MV_PAR01 := Padr(cNota	, nTamNota	)	    // Nota Inicial
		MV_PAR02 := Padr(cNota	, nTamNota	)	    // Nota Final
		MV_PAR03 := Padr(cSerie	, nTamSerie	)	    // Sï¿½rie da Nota
		MV_PAR04 := 2						        // NF de Saida
		MV_PAR05 := 1						        // Frente e Verso = Sim
		MV_PAR06 := 2						        // DANFE simplificado = Nao
		MV_PAR07 := Stod('20180101')		        // Data De
		MV_PAR08 := Stod('29991231')		        // Data Atï¿½

		oDanfe := FWMSPrinter():New(cArquivo, IMP_PDF, .F., , .T.)

		oDanfe:SetResolution(78)
		oDanfe:SetPortrait()
		oDanfe:SetPaperSize(DMPAPER_A4)
		oDanfe:SetMargin(60, 60, 60, 60)

		oDanfe:nDevice	:= 6
		oDanfe:cPathPDF := cPasta1
		oDanfe:lServer	:= .T.
		oDanfe:lInJob	:= .T.
		oDanfe:lViewPDF := .F.

		PixelX		:= oDanfe:nLogPixelX()
		PixelY		:= oDanfe:nLogPixelY()
		nConsNeg	:= 0.4
		nConsTex	:= 0.5
		oRetNF		:= Nil
		nColAux		:= 0

		U_DANFEProc(@oDanfe, , cIDEnt, Nil, Nil, .F., .F. )

		oDanfe:Print()

	Endif

	IF !FILE(xFile)
		xFile := ""
		U_KRO1041(" ROBO GRNF - [ Gerar Danfe XML ] - Houve um erro ao gerar o arquivo!")

	Endif

Return xFile
// #################################################################################################################################
Static Function fGeraNFe(_cPedido)

	Local aPvlDocS          := {}
	Local aSerie            := {}
	Local xRet              := {}
	Local aStatus           := {}
	Local nPrcVen           := 0
	Local nPosSer           := 0
	Local nX                := 1
	Local nTempoEspera      := 0
	Local nUltimoStts       := 0
	Local lOk               := .T.
	Local _cSerie           := ""
	Local cNota             := ""
	Local _cIDent           := ""
	Local _cVersao          := ""
	Local _cModal           := ""
	Local _cAmbi            := ""
	Local cTemp             := ""
	Local xAux              := ""
	Local cURL              := SuperGetMV("MV_SPEDURL", .F., "")
	Local nSegsSleep        := SuperGetMV("KR_GRNF038", .F., 1500)
	Local nQntBuscaSttsNF   := SuperGetMV("KR_GRNF039", .F., 3)
	Local cSefazStatus      := SuperGetMV("KR_GRNF03A", .F., "100/030/001/102")
	Local jRetorno          := JsonObject():New()

	Private bFiltraBrw      := {|| .T.}

	// Inicializa o objeto de retorno
	jRetorno["pedido"]      := _cPedido
	jRetorno["nf"]          := ""
	jRetorno["serie"]       := ""
	jRetorno["chave"]       := ""
	jRetorno["error"]       := ""

	U_KRO1041(" ROBO GRNF - [ Documento Saida ] - INICIANDO documento de saida...")

	aSerie := StrToKArr(SuperGetMV("MV_ESPECIE", .F., ""), ";")
	If (nPosSer := AScan(aSerie, { |x| "SPED" $ x } )) == 0
		lOk := .F.
		jRetorno["error"] := "Serie SPED nao configurada no parametro MV_ESPECIE"
		Return {lOk, jRetorno}

	Endif

	If lOk
		_cSerie := Left(aSerie[nPosSer], At("=", aSerie[nPosSer]) - 1)
		jRetorno["serie"] := _cSerie

		SC5->(DbSetOrder(1))
		If !SC5->(MsSeek( xFilial("SC5")+ _cPedido ))
			lOk := .F.
			jRetorno["error"] := "Pedido nao encontrado no banco de dados: " + _cPedido
			Return {lOk, jRetorno}

		Endif

		SC6->(dbSetOrder(1))
		SC6->(MsSeek( xFilial("SC6") + SC5->C5_NUM ))

		While SC6->(!Eof() .And. SC6->C6_FILIAL == xFilial("SC6")) .And. SC6->C6_NUM == _cPedido

			SC9->(DbSetOrder(1))
			SC9->(MsSeek(xFilial("SC9")+SC6->(C6_NUM+C6_ITEM))) //FILIAL+NUMERO+ITEM

			SE4->(DbSetOrder(1))
			SE4->(MsSeek(xFilial("SE4")+SC5->C5_CONDPAG) ) //FILIAL+CONDICAO PAGTO

			SB1->(DbSetOrder(1))
			SB1->(MsSeek(xFilial("SB1")+SC6->C6_PRODUTO)) //FILIAL+PRODUTO

			SB2->(DbSetOrder(1))
			SB2->(MsSeek(xFilial("SB2")+SC6->(C6_PRODUTO+C6_LOCAL))) //FILIAL+PRODUTO+LOCAL

			SF4->(DbSetOrder(1))
			SF4->(MsSeek(xFilial("SF4")+SC6->C6_TES)) //FILIAL+TES

			nPrcVen := SC9->C9_PRCVEN
			If ( SC5->C5_MOEDA <> 1 )
				nPrcVen := xMoeda(nPrcVen,SC5->C5_MOEDA,1,dDataBase)
			EndIf

			If Empty(SC9->C9_BLEST) .AND. Empty(SC9->C9_BLCRED)
				AAdd(aPvlDocS,{ SC9->C9_PEDIDO,;
					SC9->C9_ITEM,;
					SC9->C9_SEQUEN,;
					SC9->C9_QTDLIB,;
					nPrcVen,;
					SC9->C9_PRODUTO,;
					.F.,;
					SC9->(RecNo()),;
					SC5->(RecNo()),;
					SC6->(RecNo()),;
					SE4->(RecNo()),;
					SB1->(RecNo()),;
					SB2->(RecNo()),;
					SF4->(RecNo())})
			EndIf

			SC6->(DbSkip())
		EndDo

		If Empty(aPvlDocS)
			lOk := .F.
			jRetorno["error"] := "Nenhum item encontrado para gerar documento de saida - Pedido: " + _cPedido
			Return {lOk, jRetorno}

		Endif

		SetFunName("MATA461")
		U_KRO1041(" ROBO GRNF - [ Documento Saida ] - Processo de nota fiscal iniciado") // gera nf
		cNota := MaPvlNfs(;
            /*aPvlNfs*/         aPvlDocS,;  // 01 - Array com os itens a serem gerados
            /*cSerieNFS*/       _cSerie,;   // 02 - Serie da Nota Fiscal
            /*lMostraCtb*/      .F.,;       // 03 - Mostra Lanï¿½amento Contï¿½bil
            /*lAglutCtb*/       .F.,;       // 04 - Aglutina Lanï¿½amento Contï¿½bil
            /*lCtbOnLine*/      .F.,;       // 05 - Contabiliza On-Line
            /*lCtbCusto*/       .T.,;       // 06 - Contabiliza Custo On-Line
            /*lReajuste*/       .F.,;       // 07 - Reajuste de preï¿½o na Nota Fiscal
            /*nCalAcrs*/        0,;         // 08 - Tipo de Acrï¿½scimo Financeiro
            /*nArredPrcLis*/    0,;         // 09 - Tipo de Arredondamento
            /*lAtuSA7*/         .T.,;       // 10 - Atualiza Amarraï¿½ï¿½o Cliente x Produto
            /*lECF*/            .F.,;       // 11 - Cupom Fiscal
            /*cEmbExp*/         "",;        // 12 - Nï¿½mero do Embarque de Exportaï¿½ï¿½o
            /*bAtuFin*/         {||},;      // 13 - Bloco de Cï¿½digo para complemento de atualizaï¿½ï¿½o dos tï¿½tulos financeiros
            /*bAtuPGerNF*/      {||},;      // 14 - Bloco de Cï¿½digo para complemento de atualizaï¿½ï¿½o dos dados apï¿½s a geraï¿½ï¿½o da Nota Fiscal
            /*bAtuPvl*/         {||},;      // 15 - Bloco de Cï¿½digo de atualizaï¿½ï¿½o do Pedido de Venda antes da geraï¿½ï¿½o da Nota Fiscal
            /*bFatSE1*/         {|| .T. },; // 16 - Bloco de Cï¿½digo para indicar se o valor do Titulo a Receber serï¿½ gravado no campo F2_VALFAT quando o parï¿½metro MV_TMSMFAT estiver com o valor igual a "2".
            /*dDataMoe*/        dDatabase,; // 17 - Data da cotaï¿½ï¿½o para conversï¿½o dos valores da Moeda do Pedido de Venda para a Moeda Forte
            /*lJunta*/          .F.)        // 18 - Aglutina Pedido Iguais

		U_KRO1041(" ROBO GRNF - [ Documento Saida ] - Processo de nota fiscal finalizado")

		If Empty(cNota)
			lOk := .F.
			jRetorno["error"] := "Erro ao gerar Documento de Saida - Pedido: " + _cPedido
			Return {lOk, jRetorno}

		Endif

		jRetorno["nf"] := cNota
		U_KRO1041(" ROBO GRNF - [ Documento Saida ] - Nota fiscal gerada: " + cNota + " Serie: " + _cSerie)

		// Coleta dados do TSS.
		If Empty(_cIDent := RetIDEnti())
			lOk := .F.
			jRetorno["error"] := "Erro ao obter ID da entidade TSS - Pedido: " + _cPedido + " NF: " + cNota + " Serie: " + _cSerie
			Return {lOk, jRetorno}
		ElseIf Empty(_cVersao := GetCfgVersao(@xRet, _cIDent, "55"))
			lOk := .F.
			jRetorno["error"] := "Erro ao obter versao TSS - Pedido: " + _cPedido + " NF: " + cNota + " Serie: " + _cSerie
			Return {lOk, jRetorno}
		ElseIf Empty(_cAmbi := GetCfgAmbiente(@xRet, _cIDent, "55"))
			lOk := .F.
			jRetorno["error"] := "Erro ao obter ambiente TSS - Pedido: " + _cPedido + " NF: " + cNota + " Serie: " + _cSerie
			Return {lOk, jRetorno}
		ElseIf Empty(_cModal := GetCfgModalidade(@xRet, _cIDent, "55"))
			lOk := .F.
			jRetorno["error"] := "Erro ao obter modalidade TSS - Pedido: " + _cPedido + " NF: " + cNota + " Serie: " + _cSerie
			Return {lOk, jRetorno}
		EndIf

		U_KRO1041(" ROBO GRNF - [ Documento Saida ] - Processo de Sped Sefaz iniciado")

		SF2->(DbSetOrder(1))
		If !SF2->(MsSeek( xFilial("SF2") + cNota + _cSerie ))
			lOk := .F.
			jRetorno["error"] := "NF nao encontrada na tabela SF2 - Pedido: " + _cPedido + " NF: " + cNota + " Serie: " + _cSerie
			Return {lOk, jRetorno}

		Endif

		// transmite nf
		cTemp := SpedNFeTrf( "SF2", SF2->F2_SERIE, SF2->F2_DOC, SF2->F2_DOC, _cIDent, _cAmbi, _cModal, _cVersao, .T., , , , , , , .T. )

		U_KRO1041(" ROBO GRNF - [ Documento Saida ] - Processo de Sped Sefaz finalizado")

		If Empty(cTemp)
			lOk := .F.
			jRetorno["error"] := "Erro na transmissao SEFAZ - retorno vazio - Pedido: " + _cPedido + " NF: " + cNota + " Serie: " + _cSerie
			Return {lOk, jRetorno}

		Endif

		xAux := Substr(cTemp, RAt("Foram", cTemp)+6)
		xAux := Alltrim(Upper(Substr( xAux, 1, At(" em ", xAux) )))
		xAux := strTokArr(xAux, " ")

		If Len(xAux) < 2 .OR. xAux[1] != "TRANSMITIDAS" .OR. Val(xAux[2]) <= 0
			lOk := .F.
			jRetorno["error"] := "Erro na transmissao SEFAZ: " + cTemp + " - Pedido: " + _cPedido + " NF: " + cNota + " Serie: " + _cSerie
			Return {lOk, jRetorno}

		Endif

		nTempoEspera := (nQntBuscaSttsNF*nSegsSleep)/1000
		nTempoEspera := Iif(nTempoEspera > 1, nTempoEspera, 2)
		U_KRO1041(" ROBO GRNF - [ Documento Saida ] - Atualizacao do status da NFE / Geracao de Chave Iniciado / Pode demorar: "+ cValToChar(nTempoEspera) +" segundos")
		Sleep(2000)

		For nX := 1 To nQntBuscaSttsNF

			U_KRO1041(" ROBO GRNF - [ Documento Saida ] - Atualizacao do status da NFE / Step: " + cValToChar(nX) + "/" + cValToChar(nQntBuscaSttsNF) )

			If nX > 1 .AND. nSegsSleep > 1
				Sleep(nSegsSleep)
			EndIf

			//gera chvnfe
			aStatus := ProcMonitorDoc(RetIdEnti(), cUrl, {SF2->F2_SERIE,  SF2->F2_DOC, SF2->F2_DOC}, 1, , , @cTemp, ) //gera chvnfe

			nUltimoStts := len(aStatus)

			If Empty(aStatus)
				lOk := .F.
				jRetorno["error"] := "Nao houve retorno de status do SEFAZ - Pedido: " + _cPedido + " NF: " + cNota + " Serie: " + _cSerie
				U_KRO1041(" ROBO GRNF - [ Documento Saida ] - Atualizacao do status da NFE / Step: " + cValToChar(nX) + "/" + cValToChar(nQntBuscaSttsNF) +;
					" / Status Erro: " +  jRetorno["error"])

			ElseIf !(aStatus[nUltimoStts][5] $ cSefazStatus) // sucesso ou "Inutilizaï¿½ï¿½o de numeraï¿½ï¿½o autorizada" jï¿½ autorizado!
				lOk := .F.
				jRetorno["error"] := "Erro SEFAZ: " + AllTrim(aStatus[nUltimoStts][9]) + " - Pedido: " + _cPedido + " NF: " + cNota + " Serie: " + _cSerie
				U_KRO1041(" ROBO GRNF - [ Documento Saida ] - Atualizacao do status da NFE / Step: " + cValToChar(nX) + "/" + cValToChar(nQntBuscaSttsNF) +;
					" / Status Erro: " +  jRetorno["error"])

			Else
				lOk := .T.
				Exit // se tudo der certo, sai do "loop"

			EndIf
		Next
		U_KRO1041(" ROBO GRNF - [ Documento Saida ] - Atualizacao do status da NFE / Geracao de Chave Finalizado")

		If !lOk
			Return {lOk, jRetorno} //se deu erro durante o for, ele retorna e para tudo

		Else
			jRetorno["error"] := "" // caso nn deu erro no for, ele limpa os erros antigos de espera do status de transmissï¿½o

		EndIf

		jRetorno["chave"] := AllTrim(aStatus[nUltimoStts][6])

		// Atualiza a chave na SF2 se necessario
		SF2->(DbSetOrder(1))
		If SF2->(MsSeek( xFilial("SF2") + cNota + _cSerie ))
			If Empty(SF2->F2_CHVNFE)
				RecLock("SF2", .F.)
				SF2->F2_CHVNFE := AllTrim(aStatus[nUltimoStts][6])
				MsUnlock()

			Endif
			jRetorno["chave"] := AllTrim(SF2->F2_CHVNFE)

		Endif
	Endif

	SC5->(DbCloseArea())
	SC6->(DbCloseArea())
	SC9->(DbCloseArea())
	SE4->(DbCloseArea())
	SB1->(DbCloseArea())
	SB2->(DbCloseArea())
	SF4->(DbCloseArea())
	SF2->(DbCloseArea())

Return {lOk, jRetorno}
// #################################################################################################################################
Static Function fGeraPedido(oJson)

	Local _cDoc             := ""                                              // Nï¿½mero do Pedido de Vendas
	Local _cCliente         := Padr(Alltrim(oJson["codigo_fornecedor"]),6)	   // Cï¿½digo do Cliente
	Local _cLoja            := oJson["loja"]                                   // Loja do Cliente
	Local _cPagmnt          := oJson["condicao_pagamento"]                     // Cï¿½digo da Condiï¿½ï¿½o de Pagamento
	Local _cTipSaida        := Upper(oJson["tipo_saida"])                      // Tipo da Saida
	Local _cNumSolicit      := oJson["numero_solicitacao"]                     // Cod Solicitacao no SE
	Local _cSolicit         := oJson["solicitante"]                            // Cod Solicitante
	Local _cCodRv           := oJson["cod_rv"]                                 // Cod unidade destino OU ped compra
	Local _cCodTransp       := oJson["codigo_transportadora"]                  // Cod Transportadora
	Local _cTipVolume       := oJson["tipo_volume"]                            // Tipo do Volume
	Local _cObservSE        := oJson["observacao"]                             // Observacao do SE
	Local _cTipPedido       := oJson["tipo_pedido"]                            // Tipo do Pedido
	Local _nPeso            := Val(StrTran(oJson["peso_kg"], ",", "."))             // Peso
	Local _nQntVol          := Val(StrTran(oJson["quantidade_volume"], ",", "."))   // Volume
	Local _cProd            := Nil
	Local _cAlmx            := Nil
	Local _cTES             := Nil
	Local _nQnt             := Nil
	Local _nVal             := Nil
	Local _cMsgNFE          := ""
	Local _cTipCliente      := ""
	Local cTempMsg          := ""
	Local xRet              := ""
	Local cFilSA1           := ""
	Local cFilSB1           := ""
	Local cFilSE4           := ""
	Local cFilSF4           := ""
	Local cFileErrorLog     := ""
	Local cPathErrorLog     := "\GRNF03\Logs\"
	Local nOpcX             := 3
	Local nX                := 0
	Local aCabec            := {}
	Local aItens            := {}
	Local aLinha            := {}
	Local lOk               := .T.

	Private lMsErroAuto     := .F.
	Private lAutoErrNoFile  := .F.

	U_KRO1041( " ROBO GRNF - [GRNF03 - GERA PEDIDO] := Inicio: Inclusao pedido de Venda (" + Time() + ")" )
	SA1->(dbSetOrder(1))
	SA2->(dbSetOrder(1))
	SB1->(dbSetOrder(1))
	SE4->(dbSetOrder(1))
	SF4->(dbSetOrder(1))
	cFilAGG := xFilial("AGG")
	cFilSA1 := xFilial("SA1")
	cFilSA2 := xFilial("SA2")
	cFilSB1 := xFilial("SB1")
	cFilSE4 := xFilial("SE4")
	cFilSF4 := xFilial("SF4")

	//****************************************************************
	//  Validacoes
	//****************************************************************
	If SE4->(!MsSeek(cFilSE4 + _cPagmnt))
		cTempMsg += "Cadastrar a Condiï¿½ï¿½o de Pagamento: " + _cPagmnt + CRLF
		lOk     := .F.

	EndIf

	If _cTipPedido == "N"
		If SA1->(!MsSeek(cFilSA1 + _cCliente + _cLoja))
			cTempMsg += "Nao encontrado o cadastro do cliente: " + _cCliente + " Loja: " + _cLoja + CRLF
			lOk     := .F.

		EndIf

	Else
		If SA2->(!MsSeek(cFilSA2 + _cCliente + _cLoja))
			cTempMsg += "Nao encontrado o cadastro do fornecedor: " + _cCliente + " Loja: " + _cLoja + CRLF

			lOk     := .F.

		EndIf

	EndIf

	If _cTipPedido == "N"
		_cTipCliente := AllTrim(SA1->A1_TIPO)
		If Empty(_cTipCliente)
			cTempMsg += "Tipo do Cliente nao preenchido"
			lOk := .F.

		Endif

	Else
		_cTipCliente :=  AllTrim(SA2->A2_TIPO)
		Iif(aScan(X3CBoxToArray("C5_TIPOCLI")[2], { |x| x $ _cTipCliente }) > 0, _cTipCliente, _cTipCliente := "R")

	Endif
	//****************************************************************
	//* Inicio inclusao
	//****************************************************************
	If lOk
		_cDoc   := U_kGetSx8Num("SC5", "C5_NUM")

		DbSelectArea("SC5")
		aCabec      := {}
		aItens      := {}
		aLinha      := {}
		_cMsgNFE    := "Solicitaï¿½ï¿½o Nr " + _cNumSolicit + " - " + _cSolicit +;
			Iif(Empty(_cCodRv),     "", " - Pedido Compra: " + _cCodRv  ) +;
			Iif(Empty(_cObservSE),  "", " - " + AllTrim(_cObservSE)     )

		aAdd(aCabec, {"C5_NUM"          , _cDoc                                         , Nil})

		If _cTipSaida == "R"
			aAdd(aCabec, {"C5_TIPO"     , "B"                                           , Nil}) // Fornecedor

		Else
			If Upper(_cTipPedido) == "U" // Fornecedor
				aAdd(aCabec, {"C5_TIPO" ,    "B"                                        , Nil}) // Fornecedor

			Else
				aAdd(aCabec, {"C5_TIPO" ,    "N"                                        , Nil}) // Normal (Cliente)

			EndIf

		Endif

		aAdd(aCabec, { "C5_CLIENTE"     , PadR(_cCliente, TamSX3("C5_CLIENTE")[1])      , Nil } )
		aAdd(aCabec, { "C5_LOJACLI"     , PadR(_cLoja, TamSX3("C5_LOJACLI")[1])         , Nil } )
		aAdd(aCabec, { "C5_CONDPAG"     , PadR(_cPagmnt, TamSX3("C5_CONDPAG")[1])       , Nil } )
		aAdd(aCabec, { "C5_NATUREZ"     , PadR(101001, TamSX3("C5_NATUREZ")[1])         , Nil } )
		aAdd(aCabec, { "C5_TIPOCLI"     , PadR(_cTipCliente, TamSX3("C5_TIPOCLI")[1])   , Nil } )
		aAdd(aCabec, { "C5_TRANSP"      , PadR(_cCodTransp, TamSX3("C5_TRANSP")[1])     , Nil } )
		aAdd(aCabec, { "C5_INDPRES"     , "0"                                           , Nil } )
		aAdd(aCabec, { "C5_VOLUME4"     , _nQntVol                                      , Nil } )
		aAdd(aCabec, { "C5_ESPECI4"     , PadR(_cTipVolume,  TamSX3("C5_ESPECI4")[1])   , Nil } )
		aAdd(aCabec, { "C5_PESO4"       , _nPeso                                        , Nil } )
		aAdd(aCabec, { "C5_PEDBLQ"      , PadR("98",         TamSX3("C5_PEDBLQ")[1])    , Nil } )
		aAdd(aCabec, { "C5_DTCOTA"      , Date()                                        , Nil } )
		aAdd(aCabec, { "C5_MENNFE"      , _cMsgNFE                                      , Nil } )

		//Produtos
		For nX := 1 To Len(oJson["produtos"])

			aLinha  := {}
			_cProd  := oJson["produtos", nX, "codigo_produto"]                                                                           // Cï¿½digo do Produto
			_cAlmx  := oJson["produtos", nX, "almoxarifado"]                                                                             // Cï¿½digo do Produto
			_cTES   := oJson["produtos", nX, "tes"]                                                                                      // Cï¿½digo do TES
			_nQnt   := Val(StrTran(oJson["produtos", nX, "quantidade_produto"], ",", "."))                                               // Quantidade
			_nVal   := Val(StrTran(oJson["produtos", nX, "valor_produto"], ",", "."))                                                    // Valor

			If SB1->(! MsSeek(cFilSB1 + _cProd))
				cTempMsg += "Cadastrar o Produto: " + _cProd + CRLF
				lOk     := .F.

			EndIf
			If SF4->(! MsSeek(cFilSF4 + _cTES))
				cTempMsg += "Cadastrar o TES: " + _cTES + CRLF
				lOk     := .F.

			EndIf

			aAdd(aLinha, { "C6_ITEM"    , StrZero(nX,2)                             , Nil } )
			aAdd(aLinha, { "C6_PRODUTO" , PadR(_cProd,   TamSX3("C6_PRODUTO")[1])   , Nil } )
			aAdd(aLinha, { "C6_QTDVEN"  ,  _nQnt                                    , Nil } )
			aAdd(aLinha, { "C6_PRCVEN"  ,  _nVal                                    , Nil } )
			aAdd(aLinha, { "C6_TES"     , PadR(_cTES,    TamSX3("C6_TES")[1])       , Nil } )
			aAdd(aLinha, { "C6_LOCAL"   , PadR(_cAlmx,   TamSX3("C6_LOCAL")[1])     , Nil } )
			aAdd(aItens, aLinha)

		Next nX

		If lOk
			MSExecAuto({|a, b, c| MATA410(a, b, c)}, aCabec, aItens, nOpcX)

			If !lMsErroAuto
				xRet        := _cDoc
				U_KRO1041( " ROBO GRNF - [GRNF03 - GERA PEDIDO] := Pedido gerado com sucesso! ["+ _cDoc +"] - (" + Time() + ")" )

			Else
				lOk := .F.
				cFileErrorLog := _cDoc+"_"+DToS(Date())+"_"+Time()+".log"
				cTempMsg := MostraErro(cPathErrorLog, cFileErrorLog)

				If Empty(cTempMsg)
					cTempMsg    := "Erro: " + Alltrim(FwGetUltHlp()[2][1]) + " Solucao: " + Alltrim(FwGetUltHlp()[3][1])

				EndIf
			EndIf
		Endif
	EndIf

	If !lOk
		xRet := cTempMsg
		U_KRO1041( " ROBO GRNF - [GRNF03 - GERA PEDIDO] := Houve um erro ao gerar o Pedido de Venda (" + Time() + ") - Erro: ["+ AllTrim(cTempMsg) +"]" )

	Endif

	SC5->(DbCloseArea())
	SA1->(DBCloseArea())
	SA2->(DBCloseArea())
	SB1->(DBCloseArea())
	SE4->(DBCloseArea())
	SF4->(DBCloseArea())

Return {lOk, xRet}
// #################################################################################################################################
