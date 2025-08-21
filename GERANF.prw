#include 'protheus.ch'
#include 'parmtype.ch'
#Include "TOTVS.ch"
#Include "RESTFUL.ch"
#Include "Colors.ch"
#Include "RPTDef.ch"
#Include "FWPrintSetup.ch"
#Include "TopConn.ch"

Static Enter	:= Chr(13) + Chr(10)
/*/{Protheus.doc} WSGERANF
Ws rest para Geração de NF
@author gabriel.souza
@since 30/05/2025
@version 1.0
@type webservice
/*/
// #################################################################################################################################
WSRESTFUL WSGERANF DESCRIPTION 'WebService Geração de NF'

	//Métodos
	WSMETHOD GET	HOME	DESCRIPTION "Retorna versão da API + Dados Prod"	PATH ""			WSSYNTAX "/GET/"

	WSMETHOD POST	DANFE	DESCRIPTION "Geração da NF"						PATH "/GERANF"	WSSYNTAX "WSGERANF/GERANF/"

END WSRESTFUL
// #################################################################################################################################
WsMethod GET HOME WsService WSGERANF

	Local jResponse := JsonObject():New()

	jResponse		:= U_WSHOME() // Fonte para trazer dados simples // Uma função
	Self:SetContentType('application/json')
	Self:SetResponse(jResponse:toJSON())

Return .T.
// #################################################################################################################################
WsMethod POST DANFE WsService WSGERANF

	Local jResponse			:= JsonObject():New()
	Local jLogRet			:= JsonObject():New()
	Local oJson				:= Nil
	Local oFile				:= Nil
	Local lOk				:= .T.
	Local cBody				:= ""
	Local cMsgErro			:= ""
	Local cFile				:= ""
	Local cErroTxt			:= "Erro"
	Local aRet				:= {}
	Local aCamposObrt		:= {;
		{"codigo_fornecedor"	, .F.},;
		{"loja"					, .T.},;
		{"codigo_transportadora", .T.},;
		{"numero_nf_devolvida"	, .T.},;
		{"condicao_pagamento"	, .T.},;
		{"numero_solicitacao"	, .T.},; // Número da solicitação
		{"solicitante"			, .T.},; // Nome de quem chama a API
		{"tipo_volume"			, .T.},;
		{"quantidade_volume"	, .T.},;
		{"peso_kg"				, .T.},;
		{"cod_rv"				, .F.},;
		{"observacao"			, .F.},;
		{"tipo_pedido"			, .T.},;
		{"produtos"				, .T.},;
		{;
		{"codigo_produto"		, .T.},;
		{"quantidade_produto"	, .T.},;
		{"valor_produto"		, .T.},;
		{"almoxarifado"			, .T.},;
		{"item"					, .T.},;
		{"codigo_destino"		, .F.},;
		{"tes"					, .T.};
		}}

	Private lLogGrNF		:= Nil
	Private nTamNota		:= 0
	Private nTamSerie		:= 0
	Private cPasta1			:= ""
	Private cPasta2			:= ""
	Private xFile			:= ""
	Private cIdent			:= ""
	Private PixelX			:= 0
	Private PixelY			:= 0
	Private nConsNeg		:= 0
	Private nConsTex		:= 0
	Private nColAux			:= 0
	Private oRetNF			:= Nil
	Private _cIDent			:= ""

	oJSon					:= JsonObject():New()
	cBody					:= Self:GetContent()
	cMsgErro				:= oJson:FromJson( cBody )
	lLogGrNF				:= SuperGetMV("XX_GNFSLOG", .F., .F.)
	_cIDent					:= RetIDEnti()

	// validacoes de erro
	If Empty(cMsgErro)

		If lLogGrNF
			U_kGeraCV8( { cUserName, "WSGERNF", "[WSGERANF - GERANF]: INICIADO DO PROCESSO - (" + Time() + ") - ["+ cNumEmp +"]" }, , oJson:ToJson() )

		EndIf

		aRet		:= validJson(oJson, aCamposObrt)
		lOk			:= aRet[1]
		cMsgErro	:= aRet[2]

	Else
		lOk			:= .F.
		cMsgErro	+= " - [Erro] - Ao Parsear Json"

	Endif

	If Empty( oJson["codigo_fornecedor"] ) .And. Empty(oJson["codcliente"])
		cMsgErro	:= " - [Erro] - Favor informar Cliente ou Fornecedor "
		lOk			:= .F.

	Endif

	If lOk

		nTamNota	:= TamSX3('F2_DOC'	)[1]
		nTamSerie	:= TamSX3('F2_SERIE')[1]
		cPasta1		:= SuperGetMV("XX_GNFS036", .F., "WSGERANF\gera_nf\danfe")
		cPasta2		:= SuperGetMV("XX_GNFS037", .F., "WSGERANF\gera_nf\tmp")

		jResponse					:= U_WSHOME() // Fonte para trazer dados simples
		jResponse['pedido']			:= ""
		jResponse['nf']				:= ""
		jResponse['serie']			:= ""
		jResponse['chave']			:= ""
		jResponse['arquivo_pdf']	:= ""
		jResponse['error']			:= ""
		jResponse['arquivo']		:= ""

		If lOk

			aRet := fGeraNF(oJson)
			jResponse['pedido']			:= aRet[2]['pedido']
			jResponse['nf']				:= aRet[2]['nf']
			jResponse['serie']			:= aRet[2]['serie']
			jResponse['chave']			:= aRet[2]['chave']
			jResponse['arquivo_pdf']	:= aRet[2]['arquivo_pdf']
			jResponse['error']			:= EncodeUTF8(aRet[2]['error'])

			If aRet[1] == .T.
				cArq	:= aRet[2]["arquivo_pdf"]
				oFile	:= FwFileReader():New(cArq)

				If (oFile:Open()) .AND. File(cArq)
					// fiz uma alteração que eu não precisava mais do bas64, então desativei ele, caso precise pode inserir, é simples
					cFile					:= oFile:FullRead()
					cFile64					:= Encode64( cFile )
					jResponse['arquivo']	:= cFile64

					Self:SetResponse(jResponse)
					oFile:Close()

				Else
					cArq				:= "Nao foi possivel carregar o arquivo"
					lOk					:= .F.
					jResponse['status']	:= cErroTxt
					jResponse['error']	:= EncodeUTF8(cArq)

					Self:SetStatus( 200 )
					Self:SetResponse( jResponse )

				Endif
			Else
				cMsgErro			:= EncodeUTF8(aRet[2]['error'])
				jResponse['status']	:= cErroTxt
				lOk					:= .F.

				Self:SetStatus( 200 )
				Self:SetResponse( jResponse )

			Endif
		Endif

		If lLogGrNF
			jLogRet := jResponse
			jLogRet['arquivo'] := Iif(Empty(jLogRet['arquivo']), "", "PREENCHIDO")

			U_kGeraCV8( { cUserName, "WSGERNF", "[WSGERANF - GERANF]: FIM DO PROCESSO - (" + Time() + ") - ["+ cNumEmp +"]" }, , jLogRet:ToJson() )

		EndIf

	EndIf

	If !lOk .AND. Empty(jResponse['error'])
		jResponse['error']	:= cMsgErro
		jResponse['status']	:= EncodeUTF8(cErroTxt)
		Self:SetStatus( 400 )
		Self:SetResponse( jResponse )

	Endif

Return .T.
// #################################################################################################################################
Static Function validJson(oJson, aCamposObrt)

	Local nX		:= 1
	Local nY		:= 1
	Local lOk		:= .T.
	Local lEhObrig	:= .F.
	Local cMsgErro	:= ""
	Local cCampo	:= ""
	Local jAux		:= JsonObject():New()

	If !Empty(aCamposObrt)
		For nX := 1 to Len(aCamposObrt)
			cCampo		:= aCamposObrt[nX][1]
			lEhObrig	:= aCamposObrt[nX][2]

			If lOk == .F.
				Exit
			Endif

			If ValType( cCampo ) <> "A"
				If !oJson:HasProperty(cCampo) .OR. Iif(lEhObrig, Empty(oJson:GetJsonText(cCampo)), .F.)
					cMsgErro	:= "[Erro] - Campo nao existe ou esta vazio! " + cCampo
					lOk			:= .F.
					Exit

				Endif
			Else
				For nY := 1 to Len(aCamposObrt[nX])
					cCampo		:= aCamposObrt[nX][nY][1]
					lEhObrig	:= aCamposObrt[nX][nY][2]
					jAux		:= oJson[aCamposObrt[nX-1][1]][1] // coloca a posicao no titulo do campo

					If !jAux:HasProperty(cCampo) .OR. Iif(lEhObrig, Empty(jAux[cCampo]), .F.)
						cMsgErro	:= "[Erro] - Campo dentro de "+ cCampo +" nao existe " + Iif(lEhObrig, "ou esta vazio!: ", ": ") + cCampo
						lOk			:= .F.
						Exit

					Endif
				Next
			Endif
		Next
	Endif

Return {lOk, cMsgErro}
// #################################################################################################################################
Static Function fGeraNF(oJson)

	Local _cNfOrigem		:= StrTran(oJson["numero_nf_devolvida"], ".", "")
	Local jRetorno			:= JsonObject():New()
	Local lOk				:= .T.
	Local aAux				:= {}
	Local aRet				:= {}
	Local _cPedido			:= ""
	Local _cSerie			:= ""
	Local _cChave			:= ""
	Local _cNota			:= ""

	jRetorno["pedido"]		:= ""
	jRetorno["nf"]			:= ""
	jRetorno["serie"]		:= ""
	jRetorno["chave"]		:= ""
	jRetorno["arquivo_pdf"]	:= ""
	jRetorno["error"]		:= ""

	oJson["tipo_pedido"]	:= Upper(SubStr( oJson["tipo_pedido"], 1, 1 ))

	If !Empty(_cNfOrigem)
		aRet := fPedidDev(oJson)

	Else
		aRet := fGeraPedid(oJson)

	EndIf

	If lOk := aRet[1]

		_cPedido			:= aRet[2]
		jRetorno["pedido"]	:= _cPedido
		aRet				:= {}
		aAdd(aRet, _cPedido)
		DbSelectArea("SC6")
		SC6->(DbSetOrder(1))
		If SC6->(MsSeek( xFilial("SC6") + _cPedido ))
			If SC6->C6_QTDVEN > 0
				U_Console( " GERA NF - [WSGERANF - GERA NF] := Pedido Gerado com sucesso! [" + _cPedido + "]")
				lOk := fLiberaPedido(_cPedido)

				DbSelectArea("SC9")
				SC9->(DbSetOrder(1))
				If lOk
					If SC9->(MsSeek( xFilial("SC9") + _cPedido ))
						U_Console( " GERA NF - [WSGERANF - GERA NF] := Pedido Liberado com sucesso! [" + _cPedido + "]")

						aAux := fGeraNFe(_cPedido)

						_cSerie				:= aAux[2]["serie"]
						_cNota				:= aAux[2]["nf"]
						_cChave				:= aAux[2]["chave"]
						jRetorno["serie"]	:= _cSerie
						jRetorno["nf"]		:= _cNota
						jRetorno["chave"]	:= _cChave

						If aAux[1]

							aAux := fPrintNF(_cNota, _cSerie, _cChave)
							If aAux[1]
								jRetorno['arquivo_pdf'] := aAux[2]
								U_Console( " GERA NF - [WSGERANF - GERA NF] := Arquivo gerado / Arquivo: [" + aAux[2] + "]")

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
						aRet := fDelPed(_cPedido)
						jRetorno["error"] := "Nao foi possivel localizar o documento no banco de dados! Pedido: " + _cPedido + Iif(Empty(aRet[2]), "", "\n - " + aRet[2])
					Endif
				Else
					aRet := fDelPed(_cPedido)
					jRetorno["error"] := "Nao foi possivel liberar o pedido! Pedido: " + _cPedido + Iif(Empty(aRet[2]), "", "\n - " + aRet[2])
				Endif
			Else
				lOk := .F.
				aRet := fDelPed(_cPedido)
				jRetorno["error"] := "Pedido sem quantidade a liberar! Pedido: " + _cPedido + Iif(Empty(aRet[2]), "", "\n - " + aRet[2])
			Endif
		Else
			lOk := .F.
			jRetorno["error"] := "Pedido nao encontrado no banco de dados! Pedido: " + _cPedido
		Endif
	Else
		lOk := .F.
		jRetorno["error"] := "Erro ao criar pedido: " + Iif( Empty(aRet[2]), "", aRet[2] )
	Endif

	SC6->(DbCloseArea())
	SC9->(DbCloseArea())

Return {lOk, jRetorno}
// #################################################################################################################################
Static Function fLiberaPedido( _cPed )

	Local lTransf	:= .F.
	Local lLiber	:= .F.
	Local lRet		:= .F.

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

	Local lOk	:= .T.
	Local xRet	:= {}

	U_Console(" GERA NF - [ Print PDF ] - Iniciando processo...")
	If !ExistDir(cPasta2)
		U_Console(" GERA NF - [ Print PDF ] - Pasta nao existe... [" + cPasta2 + "]")
		FwMakeDir(cPasta2)
		U_Console(" GERA NF - [ Print PDF ] - Pasta criada! [" + cPasta2 + "]")
	Endif

	DbSelectArea("SF2")
	SF2->(DbSetOrder(1))

	If SF2->(MsSeek( xFilial("SF2") + _cDoc + _cSerie ))

		If !Empty( _cChave )
			If !File( _cChave + '.pdf')
				xRet	:= fGerPDFNF(SF2->F2_FILIAL, _cDoc, _cSerie, _cChave)
				U_Console(" GERA NF - [ Print PDF ] - Arquivo gerado! [" + xRet + "]")

			Else
				xRet	:= cPasta1 + _cChave+'.pdf'
				U_Console(" GERA NF - [ Print PDF ] - Arquivo nao existe! [" + xRet + "]")
				lOk		:= .F.
			Endif
		Else
			xRet	:= "A NF esta sem chave!"
			U_Console(" GERA NF - [ Print PDF ] - A NF esta " + xRet)
			lOk		:= .F.
		Endif
	Else
		xRet	:= "O registro nao foi achado no banco de dados!"
		U_Console(" GERA NF - [ Print PDF ] - " + xRet)
		lOk		:= .F.
	Endif
	SF2->(DbCloseArea())

Return {lOk, xRet}
// #################################################################################################################################
Static Function fGerPDFNF(xFilial, cNota, cSerie, zChave)

	Local oDanfe	:= Nil

	Default cNota	:= ""
	Default cSerie	:= ""

	U_Console(" GERA NF - [ Gerar Danfe XML ] - Iniciando processo...")
	If !ExistDir(cPasta1)
		U_Console(" GERA NF - [ Gerar Danfe XML ] - Pasta nao existe... [" + cPasta1 + "]")
		FwMakeDir(cPasta1)
		U_Console(" GERA NF - [ Gerar Danfe XML ] - Pasta criada! [" + cPasta1 + "]")

	Endif

	If !Empty(cNota)

		cIdent	:= _cIDent
		If SubStr(cPasta1, Len(cPasta1), 1) != "\"
			cPasta1 += "\"
		Endif

		cArquivo := zChave
		xFile	 := cPasta1 + cArquivo + '.pdf'
		Pergunte("NFSIGW",.F.)
		MV_PAR01 := Padr(cNota	, nTamNota	)	// Nota Inicial
		MV_PAR02 := Padr(cNota	, nTamNota	)	// Nota Final
		MV_PAR03 := Padr(cSerie	, nTamSerie	)	// Serie da Nota
		MV_PAR04 := 2							// NF de Saida
		MV_PAR05 := 1							// Frente e Verso = Sim
		MV_PAR06 := 2							// DANFE simplificado = Nao
		MV_PAR07 := Stod('20180101')			// Data De
		MV_PAR08 := Stod('29991231')			// Data Ate

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

		U_DANFEProc(@oDanfe, , cIDEnt, Nil, Nil, .F., .F. ) // fonte interno consulte tdn para mais informações

		oDanfe:Print()

	Endif

	IF !FILE(xFile)
		xFile := ""
		U_Console(" GERA NF - [ Gerar Danfe XML ] - Houve um erro ao gerar o arquivo!")

	Endif

Return xFile
// #################################################################################################################################
Static Function fGeraNFe(_cPedido)

	Local aPvlDocS			:= {}
	Local aSerie			:= {}
	Local xRet				:= {}
	Local aStatus			:= {}
	Local nPrcVen			:= 0
	Local nPosSer			:= 0
	Local nX				:= 1
	Local nTempoEspera		:= 0
	Local nUltimoStts		:= 0
	Local lOk				:= .T.
	Local _cSerie			:= ""
	Local cNota				:= ""
	Local _cVersao			:= ""
	Local _cModal			:= ""
	Local _cAmbi			:= ""
	Local cTemp				:= ""
	Local xAux				:= ""
	Local cURL				:= SuperGetMV("MV_SPEDURL", .F., "")
	Local nSegsSleep		:= SuperGetMV("XX_GNFS038", .F., 1500)
	Local nQntBuscaSttsNF	:= SuperGetMV("XX_GNFS039", .F., 3)
	Local cSefazStatus		:= SuperGetMV("XX_GNFS03A", .F., "100/030/001/102")
	Local jRetorno			:= JsonObject():New()

	Private bFiltraBrw		:= {|| .T.}

	// Inicializa o objeto de retorno
	jRetorno["pedido"]		:= _cPedido
	jRetorno["nf"]			:= ""
	jRetorno["serie"]		:= ""
	jRetorno["chave"]		:= ""
	jRetorno["error"]		:= ""

	U_Console(" GERA NF - [ Documento Saida ] - INICIANDO documento de saida...")

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

		SC6->(DbSetOrder(1))
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
			Endif

			If Empty(SC9->C9_BLEST) .AND. Empty(SC9->C9_BLCRED)
				aAdd(aPvlDocS,{ SC9->C9_PEDIDO,;
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
			Endif

			SC6->(DbSkip())
		EndDo

		If Empty(aPvlDocS)
			lOk := .F.
			jRetorno["error"] := "Nenhum item encontrado para gerar documento de saida - Pedido: " + _cPedido
			Return {lOk, jRetorno}

		Endif

		SetFunName("MATA461")
		U_Console(" GERA NF - [ Documento Saida ] - Processo de nota fiscal iniciado") // gera nf
		cNota := MaPvlNfs(;
			/*aPvlNfs*/			aPvlDocS,;	// 01 - Array com os itens a serem gerados
			/*cSerieNFS*/		_cSerie,;	// 02 - Serie da Nota Fiscal
			/*lMostraCtb*/		.F.,;		// 03 - Mostra Lanamento Contabil
			/*lAglutCtb*/		.F.,;		// 04 - Aglutina Lanamento Contabil
			/*lCtbOnLine*/		.F.,;		// 05 - Contabiliza On-Line
			/*lCtbCusto*/		.T.,;		// 06 - Contabiliza Custo On-Line
			/*lReajuste*/		.F.,;		// 07 - Reajuste de preco na Nota Fiscal
			/*nCalAcrs*/		0,;			// 08 - Tipo de Acrescimo Financeiro
			/*nArredPrcLis*/	0,;			// 09 - Tipo de Arredondamento
			/*lAtuSA7*/			.T.,;		// 10 - Atualiza Amarracao Cliente x Produto
			/*lECF*/			.F.,;		// 11 - Cupom Fiscal
			/*cEmbExp*/			"",;		// 12 - Numero do Embarque de Exportacao
			/*bAtuFin*/			{||},;		// 13 - Bloco de Ccdigo para complemento de atualizacaoo dos titulos financeiros
			/*bAtuPGerNF*/		{||},;		// 14 - Bloco de Ccdigo para complemento de atualizacaoo dos dados apos a Geração da Nota Fiscal
			/*bAtuPvl*/			{||},;		// 15 - Bloco de Ccdigo de atualizacaoo do Pedido de Venda antes da Geração da Nota Fiscal
			/*bFatSE1*/			{|| .T. },; // 16 - Bloco de Ccdigo para indicar se o valor do Titulo a Receber sera gravado no campo F2_VALFAT quando o parametro MV_TMSMFAT estiver com o valor igual a "2".
			/*dDataMoe*/		dDatabase,; // 17 - Data da cotacao para conversao dos valores da Moeda do Pedido de Venda para a Moeda Forte
			/*lJunta*/			.F.)		// 18 - Aglutina Pedido Iguais

		U_Console(" GERA NF - [ Documento Saida ] - Processo de nota fiscal finalizado")

		If Empty(cNota)
			lOk := .F.
			jRetorno["error"] := "Erro ao gerar Documento de Saida - Pedido: " + _cPedido
			Return {lOk, jRetorno}

		Endif

		jRetorno["nf"] := cNota
		U_Console(" GERA NF - [ Documento Saida ] - Nota fiscal gerada: " + cNota + " Serie: " + _cSerie)

		// Coleta dados do TSS.
		If Empty(_cIDent)
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

		Endif

		U_Console(" GERA NF - [ Documento Saida ] - Processo de Sped Sefaz iniciado")

		SF2->(DbSetOrder(1))
		If !SF2->(MsSeek( xFilial("SF2") + cNota + _cSerie ))
			lOk := .F.
			jRetorno["error"] := "NF nao encontrada na tabela SF2 - Pedido: " + _cPedido + " NF: " + cNota + " Serie: " + _cSerie
			Return {lOk, jRetorno}

		Endif

		// transmite nf
		cTemp := SpedNFeTrf( "SF2", SF2->F2_SERIE, SF2->F2_DOC, SF2->F2_DOC, _cIDent, _cAmbi, _cModal, _cVersao, .T., , , , , , , .T. )

		U_Console(" GERA NF - [ Documento Saida ] - Processo de Sped Sefaz finalizado")

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
		U_Console(" GERA NF - [ Documento Saida ] - Atualizacao do status da NFE / Geracao de Chave Iniciado / Pode demorar: "+ cValToChar(nTempoEspera) +" segundos")
		Sleep(2000)

		For nX := 1 To nQntBuscaSttsNF

			U_Console(" GERA NF - [ Documento Saida ] - Atualizacao do status da NFE / Step: " + cValToChar(nX) + "/" + cValToChar(nQntBuscaSttsNF) )

			If nX > 1 .AND. nSegsSleep > 1
				Sleep(nSegsSleep)
			Endif

			//gera chvnfe
			aStatus := ProcMonitorDoc(_cIDent, cUrl, {SF2->F2_SERIE, SF2->F2_DOC, SF2->F2_DOC}, 1, , , @cTemp, )

			nUltimoStts := len(aStatus)

			If Empty(aStatus)
				lOk := .F.
				jRetorno["error"] := "Nao houve retorno de status do SEFAZ - Pedido: " + _cPedido + " NF: " + cNota + " Serie: " + _cSerie
				U_Console(" GERA NF - [ Documento Saida ] - Atualizacao do status da NFE / Step: " + cValToChar(nX) + "/" + cValToChar(nQntBuscaSttsNF) +;
					" / Status Erro: " + jRetorno["error"])

			ElseIf !(aStatus[nUltimoStts][5] $ cSefazStatus) // sucesso ou "Inutilizacao de numeracao autorizada" ja autorizado!
				lOk := .F.
				jRetorno["error"] := "Erro SEFAZ: " + AllTrim(aStatus[nUltimoStts][9]) + " - Pedido: " + _cPedido + " NF: " + cNota + " Serie: " + _cSerie
				U_Console(" GERA NF - [ Documento Saida ] - Atualizacao do status da NFE / Step: " + cValToChar(nX) + "/" + cValToChar(nQntBuscaSttsNF) +;
					" / Status Erro: " + jRetorno["error"])

			Else
				lOk := .T.
				Exit // se tudo der certo, sai do "loop"

			Endif
		Next
		U_Console(" GERA NF - [ Documento Saida ] - Atualizacao do status da NFE / Geracao de Chave Finalizado")

		If !lOk
			Return {lOk, jRetorno} //se deu erro durante o for, ele retorna e para tudo

		Else
			jRetorno["error"] := "" // caso nn deu erro no for, ele limpa os erros antigos de espera do status de transmissao

		Endif

		jRetorno["chave"] := AllTrim(aStatus[nUltimoStts][6])

		// Atualiza a chave na SF2 se necessario
		SF2->(DbSetOrder(1))
		If SF2->(MsSeek( xFilial("SF2") + cNota + _cSerie ))
			If Empty(SF2->F2_CHVNFE)

				Begin Transaction
					If RecLock("SF2", .F.)
						SF2->F2_CHVNFE := AllTrim(aStatus[nUltimoStts][6])
						SF2->(MsUnlock())

					Endif

				End Transaction

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
Static Function fGeraPedid(oJson)

	Local _cDoc				:= ""													// Número do Pedido de Vendas
	Local _cCliente			:= Padr(Alltrim(oJson["codigo_fornecedor"]),6)			// Código do Cliente
	Local _cLoja			:= oJson["loja"]										// Loja do Cliente
	Local _cPagmnt			:= oJson["condicao_pagamento"]							// Código da Condição de Pagamento
	Local _cNumSolicit		:= oJson["numero_solicitacao"]							// Cod Solicitacao no SE
	Local _cSolicit			:= oJson["solicitante"]									// Cod Solicitante
	Local _cCodRv			:= oJson["cod_rv"]										// Cod unidade destino OU ped compra
	Local _cCodTransp		:= oJson["codigo_transportadora"]						// Cod Transportadora
	Local _cTipVolume		:= oJson["tipo_volume"]									// Tipo do Volume
	Local _cObservSE		:= DecodeUTF8(oJson["observacao"])						// Observacao do SE
	Local _cTipPedido		:= oJson["tipo_pedido"]									// Tipo do Pedido
	Local _nPeso			:= Val(StrTran(oJson["peso_kg"], ",", "."))				// Peso
	Local _nQntVol			:= Val(StrTran(oJson["quantidade_volume"], ",", "."))	// Volume
	Local _cProd			:= Nil
	Local _cAlmx			:= Nil
	Local _cTES				:= Nil
	Local _nQnt				:= Nil
	Local _nVal				:= Nil
	local lMsblql			:= .T.
	Local _cMsgNFE			:= ""
	Local _cTipCliente		:= ""
	Local cTempMsg			:= ""
	Local xRet				:= ""
	Local cFilSA1			:= ""
	Local cFilSB1			:= ""
	Local cFilSE4			:= ""
	Local cFilSF4			:= ""
	Local cFileErrorLog		:= ""
	Local cPathErrorLog		:= "\WSGERANF\Logs\Pedido"
	Local nOpcX				:= 3
	Local nX				:= 0
	Local aCabec			:= {}
	Local aItens			:= {}
	Local aLinha			:= {}
	Local lOk				:= .T.
	Local cErro				:= ""
	Local nAt1				:= 0
	Local nAt2				:= 0

	Private lAutoErrNoFile	:= .F.
	Private lMsErroAuto		:= .F.

	U_Console( " GERA NF - [WSGERANF - GERA PEDIDO] := Inicio: Inclusao pedido de Venda (" + Time() + ")" )
	SA1->(DbSetOrder(1))
	SA2->(DbSetOrder(1))
	SB1->(DbSetOrder(1))
	SE4->(DbSetOrder(1))
	SF4->(DbSetOrder(1))

	cFilAGG := xFilial("AGG")
	cFilSA1 := xFilial("SA1")
	cFilSA2 := xFilial("SA2")
	cFilSB1 := xFilial("SB1")
	cFilSE4 := xFilial("SE4")
	cFilSF4 := xFilial("SF4")

	If Empty( _cCliente )
		_cCliente	:= Padr(Alltrim(oJson["codcliente"]),6)

	Endif

	//****************************************************************
	//	Validacoes
	//****************************************************************
	If _cTipPedido == "N"
		If SA1->(!MsSeek(cFilSA1 + _cCliente + _cLoja))
			cTempMsg	+= "Nao encontrado o cadastro do cliente: " + _cCliente + " Loja: " + _cLoja + CRLF
			lOk			:= .F.

		Endif

	Else
		If SA2->(!MsSeek(cFilSA2 + _cCliente + _cLoja))
			cTempMsg	+= "Nao encontrado o cadastro do fornecedor: " + _cCliente + " Loja: " + _cLoja + CRLF
			lOk			:= .F.

		Endif

	Endif

	If lOk
		If _cTipPedido == "N"
			_cTipCliente := AllTrim(SA1->A1_TIPO)
			lMsblql	:= Iif(SA1->A1_MSBLQL $ "1", .T., .F.)
			If Empty(_cTipCliente)
				cTempMsg += "Tipo do Cliente nao preenchido"
				lOk := .F.

			Endif

		Else
			_cTipCliente := AllTrim(SA2->A2_TIPO)
			lMsblql	:= Iif(SA2->A2_MSBLQL $ "1", .T., .F.)
			Iif(aScan(X3CBoxToArray("C5_TIPOCLI")[2], { |x| x $ _cTipCliente }) > 0, _cTipCliente, _cTipCliente := "R")

		Endif
	Endif

	If lMsblql
		cTempMsg += "Cliente/Forncedor esta bloqueado!"
		lOk := .F.
	EndIf

	If lOk
		If SE4->(!MsSeek(cFilSE4 + _cPagmnt))
			cTempMsg	+= "Cadastrar a Condicao de Pagamento: " + _cPagmnt + CRLF
			lOk			:= .F.

		Endif
	Endif

	//****************************************************************
	//* Inicio inclusao
	//****************************************************************
	If lOk
		_cDoc	:= GetSxeNum("SC5", "C5_NUM")

		DbSelectArea("SC5")
		aCabec		:= {}
		aItens		:= {}
		aLinha		:= {}
		_cMsgNFE	:= "Solicitacao Nr " + _cNumSolicit + " - " + _cSolicit +;
			Iif(Empty(_cCodRv)		, "", " - Pedido Compra: " + _cCodRv ) 	+;
			Iif(Empty(_cObservSE)	, "", " - " + AllTrim(_cObservSE) )		+;
			fPrdsUndDst(oJson)

		SC5->(DbSetOrder(1))
		While SC5->( MsSeek(xFilial("SC5") + _cDoc ) ) // valida c5_num
			ConfirmSx8()
			_cDoc := GetSxeNum( "SC5" , "C5_NUM" )
			SC5->( DbSkip() )

		EndDo
		aAdd(aCabec, {"C5_NUM"			, _cDoc		, Nil})

		If Upper(_cTipPedido) $ "U"
			aAdd(aCabec, {"C5_TIPO" 	, "B"		, Nil}) // Fornecedor

		ElseIf Upper(_cTipPedido) $ "D" // Dev.Compra
			aAdd(aCabec, {"C5_TIPO" 	, "D"		, Nil}) // Fornecedor

		Else
			aAdd(aCabec, {"C5_TIPO" 	, "N"		, Nil}) // Normal (Cliente)

		Endif


		aAdd(aCabec, { "C5_CLIENTE"	, PadR(_cCliente, TamSX3("C5_CLIENTE")[1])		, Nil } )
		aAdd(aCabec, { "C5_LOJACLI"	, PadR(_cLoja, TamSX3("C5_LOJACLI")[1])			, Nil } )
		aAdd(aCabec, { "C5_CONDPAG"	, PadR(_cPagmnt, TamSX3("C5_CONDPAG")[1])		, Nil } )
		aAdd(aCabec, { "C5_NATUREZ"	, PadR(101001, TamSX3("C5_NATUREZ")[1])			, Nil } )
		aAdd(aCabec, { "C5_TIPOCLI"	, PadR(_cTipCliente, TamSX3("C5_TIPOCLI")[1])	, Nil } )
		aAdd(aCabec, { "C5_TRANSP"	, PadR(_cCodTransp, TamSX3("C5_TRANSP")[1])		, Nil } )
		aAdd(aCabec, { "C5_INDPRES"	, "0"											, Nil } )
		aAdd(aCabec, { "C5_VOLUME4"	, _nQntVol										, Nil } )
		aAdd(aCabec, { "C5_ESPECI4"	, PadR(_cTipVolume, TamSX3("C5_ESPECI4")[1])	, Nil } )
		aAdd(aCabec, { "C5_PESO4"	, _nPeso										, Nil } )
		aAdd(aCabec, { "C5_PEDBLQ"	, PadR("98", TamSX3("C5_PEDBLQ")[1])			, Nil } )
		aAdd(aCabec, { "C5_DTCOTA"	, Date()										, Nil } )
		aAdd(aCabec, { "C5_MENNFE"	, cValToChar(_cMsgNFE)							, Nil } )
		aAdd(aCabec, { "C5_ORIGEM"	, "WSGERANF"									, Nil } )

		//Produtos
		For nX := 1 To Len(oJson["produtos"])

			aLinha	:= {}
			_cProd	:= oJson["produtos", nX, "codigo_produto"]								// Ccdigo do Produto
			_cAlmx	:= oJson["produtos", nX, "almoxarifado"]								// Ccdigo do Produto
			_cTES	:= oJson["produtos", nX, "tes"]											// Ccdigo do TES
			_nQnt	:= Val(StrTran(oJson["produtos", nX, "quantidade_produto"], ",", "."))	// Quantidade
			_nVal	:= Val(StrTran(oJson["produtos", nX, "valor_produto"], ",", "."))		// Valor

			If SB1->(! MsSeek(cFilSB1 + _cProd))
				cTempMsg += "Cadastrar o Produto: " + _cProd + CRLF
				lOk		:= .F.

			Endif
			If SF4->(! MsSeek(cFilSF4 + _cTES))
				cTempMsg += "Cadastrar o TES: " + _cTES + CRLF
				lOk		:= .F.

			Endif

			aAdd(aLinha, { "C6_ITEM"	, StrZero(nX,2)								, Nil } )
			aAdd(aLinha, { "C6_PRODUTO" , PadR(_cProd, TamSX3("C6_PRODUTO")[1])		, Nil } )
			aAdd(aLinha, { "C6_QTDVEN"	, _nQnt										, Nil } )
			aAdd(aLinha, { "C6_PRCVEN"	, _nVal										, Nil } )
			aAdd(aLinha, { "C6_TES"		, PadR(_cTES, TamSX3("C6_TES")[1])			, Nil } )
			aAdd(aLinha, { "C6_LOCAL"	, PadR(_cAlmx, TamSX3("C6_LOCAL")[1])		, Nil } )
			aAdd(aItens, aLinha)

		Next nX

		If lOk
			MSExecAuto({|a, b, c| MATA410(a, b, c)}, aCabec, aItens, nOpcX)

			If !lMsErroAuto
				xRet	:= _cDoc
				U_Console( " GERA NF - [WSGERANF - GERA PEDIDO] := Pedido gerado com sucesso! ["+ _cDoc +"] - (" + Time() + ")" )

			Else
				lOk := .F.
				cFileErrorLog := DToS(Date())+"_"+StrTran( Time() , ":", "-")+".log"
				cErro := MostraErro(cPathErrorLog, cFileErrorLog)

				If Empty(cErro)
					cTempMsg	:= "Erro: " + Alltrim(FwGetUltHlp()[2][1]) + " Solucao: " + Alltrim(FwGetUltHlp()[3][1])
				Else
					nAt1 := At("AJUDA:", cErro)
					nAt2 := At("Tabela SC", cErro)-nAt1
					cTempMsg := StrTran(SubStr(cErro, nAt1, nAt2), Chr(13)+Chr(10))

				Endif
			Endif
		Endif
	Endif

	If !lOk
		xRet := cTempMsg
		U_Console( " GERA NF - [WSGERANF - GERA PEDIDO] := Houve um erro ao gerar o Pedido de Venda (" + Time() + ") - Erro: ["+ AllTrim(cTempMsg) +"]" )

	Endif

	SC5->(DbCloseArea())
	SA1->(DBCloseArea())
	SA2->(DBCloseArea())
	SB1->(DBCloseArea())
	SE4->(DBCloseArea())
	SF4->(DBCloseArea())

Return {lOk, xRet}
// #################################################################################################################################
Static Function fPedidDev(oJson)

	Local lOK			:= .T.
	Local _cFornece		:= PadR(oJson['codigo_fornecedor'], 	TamSX3("F1_FORNECE")[1]	)
	Local _cLoja		:= PadR(oJson['loja'], 					TamSX3("F1_LOJA")[1]	)
	Local _cDoc			:= PadR(oJson['numero_nf_devolvida'],	TamSX3("F1_DOC")[1]		)
	Local _cObservSE	:= DecodeUTF8(oJson["observacao"])						// Observacao SE
	Local _cNumSolicit	:= oJson["numero_solicitacao"]							// Cod Solicitacao no SE
	Local _cSolicit		:= oJson["solicitante"]									// Cod Solicitante
	Local _cCodRv		:= oJson["cod_rv"]										// Cod unidade destino OU ped compra
	Local _cCodTransp	:= oJson["codigo_transportadora"]						// Cod Transportadora
	Local _cTipVolume	:= oJson["tipo_volume"]									// Tipo do Volume
	Local _cPagmnt		:= oJson["condicao_pagamento"]							// Condicao de Pagamento
	Local _cTipPedido	:= oJson["tipo_pedido"]
	Local _nPeso		:= Val(StrTran(oJson["peso_kg"], 			",", "."))	// Peso
	Local _nQntVol		:= Val(StrTran(oJson["quantidade_volume"], 	",", "."))	// Volume
	Local _cMsgNFE		:= ""
	Local _cProd		:= Nil
	Local _nQnt			:= Nil
	Local _cItem		:= Nil
	Local _cTes			:= Nil
	Local nRecNf		:= Nil
	Local xRet			:= Nil
	Local aRet			:= {}
	Local aRecItem		:= {}
	Local nX			:= 1

	U_Console( " GERA NF - [WSGERANF - GERA PEDIDO] := Inicio: Inclusao pedido de Venda (" + Time() + ")" )

	SF1->(DbSetOrder(2))
	SE4->(DbSetOrder(1))
	SA2->(DbSetOrder(1))
	SD1->(DbSetOrder(1))

	_cMsgNFE	:= "Solicitacao Nr " + _cNumSolicit + " - " + _cSolicit +;
		Iif(Empty(_cCodRv)		, "", " - Pedido Compra: " + _cCodRv )  +;
		Iif(Empty(_cObservSE)	, "", " - " + AllTrim(_cObservSE) )		+;
		fPrdsUndDst(oJson)

	If SE4->(!MsSeek(xFilial("SE4") + _cPagmnt))
		xRet	+= "Cadastrar a Condicao de Pagamento: " + _cPagmnt + CRLF
		Return {.F., xRet}

	Endif

	aParms := {_cMsgNFE, _cCodTransp, _cTipVolume, _cPagmnt, _nQntVol, _nPeso, Iif(_cTipPedido $ "U", "B", _cTipPedido)}

	// #########################################################################
	// valida cadastro fornecedor
	// #########################################################################

	If SA2->(MsSeek( xFilial("SA2") + _cFornece ))
		If SA2->A2_MSBLQL $ '1' 	// Valida se ta bloqueado
			xRet := "Fornecedor esta bloqueado! Fornece/Loja: " +;
				AllTrim(_cFornece) + "/" + AllTrim(_cLoja) + " Doc: " + AllTrim(_cDoc) + "Emp/Fil" + cNumEmp

			U_Console( " GERA NF - [WSGERANF - GERA PEDIDO] := Houve um erro ao gerar o Pedido de Venda (" + Time() + ") - Erro: ["+ AllTrim(xRet) +"]" )
			Return {.F., xRet}

		EndIf
	Else
		xRet := "Nao foi possivel localizar o fornecedor! Fornece/Loja: " +;
			AllTrim(_cFornece) + "/" + AllTrim(_cLoja) + " Doc: " + AllTrim(_cDoc) + "Emp/Fil" + cNumEmp

		U_Console( " GERA NF - [WSGERANF - GERA PEDIDO] := Houve um erro ao gerar o Pedido de Venda (" + Time() + ") - Erro: ["+ AllTrim(xRet) +"]" )
		Return {.F., xRet}

	EndIf

	// #########################################################################
	// Gera pedido
	// #########################################################################

	If lOk
		If lOk := SF1->(MsSeek( xFilial("SF1") + _cFornece + _cLoja + _cDoc )) .AND. SF1->(!Eof())
			nRecNf := SF1->(Recno()) // Coleta Recno da NFE

			For nX := 1 to Len(oJson['produtos'])

				// salva o codigo em variavel
				_cProd 	:= PadR(oJson['produtos'		, nX, 'codigo_produto']		, TamSX3("C6_PRODUTO")[1]	)	// Ccdigo do Produto
				_cItem 	:= PadL(oJson['produtos'		, nX, 'item']				, TamSX3("D1_ITEM")[1]		)	// Item
				_cTes 	:= PadR(oJson['produtos'		, nX, 'tes']				, TamSX3("C6_TES")[1]		)	// Tes
				_cAlmx	:= PadR(oJson["produtos"		, nX, "almoxarifado"]		, TamSX3("C6_LOCAL")[1]		)	// Almx
				_nQnt	:= Val(StrTran(oJson['produtos'	, nX, "quantidade_produto"]	, ",", ".")					)	// Quantidade

				// seek no produto x da nota x
				If SD1->(MsSeek( xFilial("SD1") + SF1->F1_DOC + SF1->F1_SERIE + SF1->F1_FORNECE + SF1->F1_LOJA + _cProd + _cItem )) .AND. SD1->(!Eof())

					// se achou salva o recno
					Aadd(aRecItem, {SD1->D1_ITEM, _nQnt, _cTes, _cAlmx} )
				EndIf
			Next

			If Empty(aRecItem)
				lOk := .F.
				xRet := "Nao foi possivel localizar ITEMS da NF de Origem! Fornece/Loja: " +;
					AllTrim(_cFornece) + "/" + AllTrim(_cLoja) + " Doc/Serie: " + AllTrim(SF1->F1_DOC) + "/" + AllTrim(SF1->F1_SERIE) + " Emp/Fil: " + cNumEmp
			Else
				aRet := U_fPedDev(nRecNf, aRecItem, aParms)

				If lOk := !Empty(aRet[1])
					xRet := aRet[1] // se n tem erro
				Else
					xRet := aRet[2] // se tem erro
				EndIf

			EndIf
		Else
			xRet := "Nao foi possivel localizar a NF de Origem! Fornece/Loja: " +;
				AllTrim(_cFornece) + "/" + AllTrim(_cLoja) + " Doc: " + AllTrim(_cDoc) + " Emp/Fil: " + cNumEmp
		EndIf
	EndIf

	If !lOk
		U_Console( " GERA NF - [WSGERANF - GERA PEDIDO] := Houve um erro ao gerar o Pedido de Venda (" + Time() + ") - Erro: ["+ AllTrim(xRet) +"]" )

	Endif

	SE4->(DBCloseArea())
	SF1->(DBCloseArea())
	SD1->(DBCloseArea())

Return {lOk, xRet}
// #################################################################################################################################
/*/{Protheus.doc} fDelPed
	(Concatena informacoes do Cod de Origem/Cod de Destino dos produtos em uma string)
	@type Static Function
	@author gabriel.souza
	@since 11/08/2025
	@version 1.2
	@param oJson, J, Json
	@return Array, A, Retorna Array[1] sendo logico para sucesso ou erro, e Array[2] para mensagem de erro
/*/
Static Function fDelPed(cNum)

	Local aCab				:= {}
	Local aItens			:= {}
	Local cFileErrorLog		:= ""
	Local cPathErrorLog		:= "\WSGERANF\Logs\Pedido"

	Private lMsErroAuto    	:= .F.
	Private lAutoErrNoFile 	:= .F.

	SC5->(DbSetOrder(1))
	SC6->(DbSetOrder(1))

	If SC5->(MsSeek( xFilial("SC5") + cNum ))

		aadd(aCab, { "C5_NUM",    	cNum					, Nil })
		aadd(aCab, { "C5_TIPO",    	SC5->C5_TIPO			, Nil })
		aadd(aCab, { "C5_CLIENTE", 	SC5->C5_CLIENTE			, Nil })
		aadd(aCab, { "C5_LOJACLI", 	SC5->C5_LOJACLI			, Nil })
		aadd(aCab, { "C5_LOJAENT", 	SC5->C5_LOJAENT			, Nil })
		aadd(aCab, { "C5_CONDPAG", 	SC5->C5_CONDPAG			, Nil })

		MSExecAuto({ |a, b, c| MATA410(a, b, c) }, aCab, aItens, 5)

		If lMsErroAuto
			cFileErrorLog := DToS(Date())+"_"+StrTran( Time() , ":", "-")+".log"
			MostraErro(cPathErrorLog, cFileErrorLog)

			SC5->(DbCloseArea())
			U_Console( " GERA NF - [WSGERANF - DEL PEDIDO] := Houve um erro ao excluir o pedido! LOG:["+ cPathErrorLog + cFileErrorLog +"] (" + Time() + ")" )
			Return {.F., "Houve um erro ao excluir o pedido! LOG:["+ cPathErrorLog + cFileErrorLog +"]"}
		EndIf
	Else
		SC5->(DbCloseArea())
		U_Console( " GERA NF - [WSGERANF - DEL PEDIDO] := Nao encontrado pedido para deletar [" + cNum + "] (" + Time() + ")" )
		Return {.F., "Nao encontrado pedido para deletar [" + cNum + "]"}
	EndIf

	SC5->(DbCloseArea())
	U_Console( " GERA NF - [WSGERANF - DEL PEDIDO] := Deletado com sucesso pedido de venda [" + cNum + "] (" + Time() + ")" )

Return {.T., ""}
// #################################################################################################################################
/*/{Protheus.doc} fPrdsUndDst
	(Concatena informacoes do Cod de Origem/Cod de Destino dos produtos em uma string)
	@type  Static Function
	@author gabriel.souza
	@since 11/08/2025
	@version 1.2
	@param oJson, J, Json
	@return cRet, C, retorna a concatenacao dos campos
/*/
Static Function fPrdsUndDst(oJson)

	Local cRet 			:= ""
	Local nX			:= 1
	Local lAllEmpt 		:= .T.

	If Len(oJson['produtos']) > 0
		cRet += " Cod. Peds Ref.: " + CHR(13)+CHR(10)
	EndIf

	For nX := 1 To Len(oJson['produtos'])
		If !Empty(oJson['produtos', nX, 'codigo_destino'])
			cRet += "Orig/Dest: " + oJson['produtos', nX, 'codigo_produto'] + "/" + oJson['produtos', nX, 'codigo_destino'] + CHR(13)+CHR(10)
			If lAllEmpt
				lAllEmpt := .F.
			EndIf
		EndIf
	Next

	If lAllEmpt
		cRet := ""
	EndIf

Return cRet
// #################################################################################################################################
/*/{Protheus.doc} User Function fPedDev
(ExecAuto de Retornar separado)
@type Function
@author gabriel.souza
@since 30/07/2025
/*/
User Function fPedDev( nRecno , aRecSD1, aPars )

	local lIndSB6 		:= SuperGetMv("XX_INDSB6",.F.,.F.)
	Local cPathLog		:= "\WSGERANF\LOGS\DEVOLUCAO\"
	Local aArea			:= FWGetArea()
	Local cItem			:= "00"
	Local aItens		:= {}
	Local aRet			:= {}
	Local cTempMsg		:= ""
	Local cFileLog 		:= ""
	Local cNumPed		:= ""
	Local cNumSeq 		:= ""
	Local cRecSD1		:= ""
	Local cQuery		:= ""
	Local cTes			:= ""
	Local cAlmx			:= ""
	Local cErro			:= ""
	Local nDesc			:= 0
	Local nValUnit		:= 0
	Local nPrcVen		:= 0
	Local nValDesc		:= 0
	Local nQntD1		:= 0
	Local nAt1			:= 0
	Local nAt2			:= 0
	Local nX

	//	Variaveis private do execauto
	Private aRotina		:= FWLoadMenuDef("MATA410")
	Private lMsErroAuto := .F.
	Private lUsaForn	:= .T.
	Private l410Auto	:= .T.
	Private INCLUI		:= .T.

	Default aRecSD1		:= {}
	Default aPars		:= {}
	Default nRecno		:= 0

	If nRecno == 0
		Return .F.

	Endif

	If Empty(aPars)
		Return .F.

	EndIf

	If !ExistDir( cPathLog )
		MakeDir( cPathLog , , .T. )

	Endif

	If Len( aRecSD1 ) > 0

		For nX := 1 To Len( aRecSD1 )

			If nX < Len( aRecSD1 )
				cRecSD1	+= aRecSD1[nX][1] + "','"

			Else
				cRecSD1	+= aRecSD1[nX][1]

			Endif

		Next nX

	Endif

	DbSelectArea("SF1")
	SF1->( DbGoTo( nRecno ) )

	//	Busca os documentos que serão retornados
	cQuery	:= MontaQry( nRecno , cRecSD1 )

	TCQuery cQuery New Alias "QRY"

	While (.T.)

		DbSelectArea("SC5")
		cNumPed	:= GetSx8Num("SC5","C5_NUM")
		If SC5->(DbSeek(xFilial("SC5") + cNumPed ))
			If (__lSX8)
				ConfirmSX8()

			Endif

			Loop

		Endif
		Exit

	EndDo

	//Se tem dados
	If ! QRY->(Eof())

		aCab	:= {	{	"C5_NUM"		, cNumPed				, Nil	}	,;	//  Numero
			/*	*/		{	"C5_TIPO"		, aPars[7]				, '.T.'	}	,;	//	Tipo
			/*	*/		{	"C5_VEND1"		, "000001"				, '.T.'	}	,;	//	Vendedor
			/*	*/		{	"C5_CLIENTE"	, QRY->A2_COD			, Nil	}	,;	//	Cliente
			/*	*/		{	"C5_CLIENT"		, QRY->A2_COD			, Nil	}	,;	//	Cliente
			/*	*/		{	"C5_LOJAENT"	, QRY->A2_LOJA			, Nil	}	,;	//	Loja
			/*	*/		{	"C5_LOJACLI"	, QRY->A2_LOJA			, Nil	}	,;	//	Loja
			/*	*/		{	"C5_TIPOCLI"	, "R"					, Nil	}	,;	//	Tipo
			/*	*/		{	"C5_NATUREZ"	, "101001"				, Nil	}	,;	//	Natureza
			/*	*/		{	"C5_TRANSP"		, aPars[2]				, Nil	}	,;	//	Cod Transp
			/*	*/		{	"C5_ESPECI4"	, aPars[3]				, Nil	}	,;	//	Especie
			/*	*/		{	"C5_CONDPAG"	, aPars[4]				, Nil	}	,;	//	Condicao Pagamento
			/*	*/		{	"C5_VOLUME4"	, aPars[5]				, Nil	}	,;	//	Volume
			/*	*/		{	"C5_PESO4"		, aPars[6]				, Nil	}	,;	//	Peso
			/*	*/		{	"C5_EMISSAO"	, Date()				, Nil	}	,;	//	Emissao
			/*	*/		{	"C5_TABELA"		, " "					, Nil	}	,;	//	Tabela de Preco
			/*	*/		{	"C5_TPFRETE"	, "F"					, NIL	}	,;	//	Tipo de frete
			/*	*/		{	"C5_MENNFE"		, cValToChar(aPars[1])	, NIL	}	,;	//	Observacao
			/*	*/		{	"C5_INDPRES"	, "0"					, Nil	}	,;	//
			/*	*/		{	"C5_KORIGEM"	, "1"					, Nil	}	,;	//
			/*	*/		{	"C5_ORIGEM"		, "WSSOFTNFE"			, Nil	}	,;	//
			/*	*/		{	"C5_DTCOTA"		, Date()				, Nil	} 	,;	//
			/*	*/		{	"C5_DTENT"		, Date()				, Nil	} 	}

		// Percorre os dados e vai montando o filtro
		While !QRY->(EoF())

			nDesc		:= 0
			nValUnit	:= 0
			nQntD1		:= 0
			nPrcVen		:= 0
			nValDesc	:= 0
			nValTot		:= 0
			DbSelectArea("SB1")
			SB1->(DbSetOrder(1))
			If SB1->(DbSeek( xFilial("SB1") + QRY->D1_COD ))

				cItem	:= Soma1(cItem)

				nPos	:= aScan( aRecSD1 , { |x| x[1] == QRY->D1_ITEM } )

				If nPos > 0

					If !Empty(aRecSD1[nPos][3]) // TES
						cTes 		:= aRecSD1[nPos][3]

					Else
						cTes		:= QRY->F4_CODIGO

					EndIf

					If !Empty(aRecSD1[nPos][2]) // QNT
						nQtde 		:= aRecSD1[nPos][2]

					Else
						nQtde		:= QRY->D1_QUANT

					EndIf

					If !Empty(aRecSD1[nPos][4]) // ALMX
						cAlmx		:= aRecSD1[nPos][4]

					Else
						cAlmx		:= QRY->D1_LOCAL

					EndIf

					cCFOP	:= fRetCfop( QRY->A2_COD , QRY->A2_LOJA , cTes , "S" , aPars[7] )

					SB1->(DbSeek( xFilial("SB1") + QRY->D1_COD ))

					If lIndSb6
						DbSelectArea("SB6")
						SB6->(DbSetOrder(6))
						If SB6->(MsSeek( xFilial("SB6") + QRY->A2_COD + QRY->A2_LOJA + QRY->D1_SERIE + QRY->D1_DOC + QRY->D1_ITEM ))
							cNumSeq	:= SB6->B6_IDENT

						EndIf

					Else
						cNumSeq		:= GetSeqB6(QRY->A2_COD, QRY->A2_LOJA, QRY->D1_SERIE, QRY->D1_DOC, QRY->D1_ITEM)

					Endif

					If QRY->D1_VALDESC > 0

						nDesc		:= QRY->D1_VALDESC
						nValUnit 	:= QRY->D1_VUNIT
						nQntD1		:= QRY->D1_QUANT

						nPrcVen		:= ((((nValUnit*nQntD1)-nDesc)/nQntD1)*nQtde)/nQtde
						nValDesc 	:= (nDesc/nQntD1)*nQtde
						nPrcVen		:= Round(nPrcVen 					, GetSX3Cache("C6_PRCVEN" ,	"X3_DECIMAL") )
						nValTot		:= Round((nValUnit*nQtde)-nValDesc	, GetSX3Cache("C6_VALOR" ,	"X3_DECIMAL") )

					Else
						nPrcVen		:= Round(QRY->D1_VUNIT			, GetSX3Cache("C6_PRCVEN" ,	"X3_DECIMAL") )
						nValTot		:= Round(QRY->D1_VUNIT*nQtde	, GetSX3Cache("C6_VALOR" ,	"X3_DECIMAL") )

					EndIf

					aTemp := {}

					aAdd( aTemp , {"C6_ITEM"	, cItem				, Nil }	) //	Item do Pedido
					aAdd( aTemp , {"C6_PRODUTO"	, SB1->B1_COD		, Nil }	) //	Codigo do Produto
					aAdd( aTemp , {"C6_UM"		, SB1->B1_UM		, Nil }	) //	Unidade de Medida
					aAdd( aTemp , {"C6_SERIORI"	, QRY->D1_SERIE		, Nil }	) //	SERIE 	ORI
					aAdd( aTemp , {"C6_NFORI"	, QRY->D1_DOC		, Nil }	) //	NF 		ORI
					aAdd( aTemp , {"C6_ITEMORI"	, QRY->D1_ITEM		, Nil }	) //	ITEM 	ORI
					aAdd( aTemp , {"C6_LOCAL"	, cAlmx				, Nil }	) //	Armazem
					aAdd( aTemp , {"C6_QTDVEN"	, nQtde				, Nil }	) //	Quantidade do Item
					aAdd( aTemp , {"C6_PRCVEN"	, nPrcVen			, Nil }	) //	Preco tabela
					aAdd( aTemp , {"C6_PRUNIT"	, QRY->D1_VUNIT		, Nil }	) //	Preco Lista
					aAdd( aTemp , {"C6_TES"		, cTes				, Nil }	) //	Tipo de Entrada/Saida do Item
					aAdd( aTemp , {"C6_CF"		, cCFOP				, Nil }	) //	CFOP
					aAdd( aTemp , {"C6_ENTREG"	, Date()			, Nil }	) //	Data entrega
					aAdd( aTemp , {"C6_NUM"		, cNumPed			, Nil }	) //	Numero do Pedido
					If !Empty(cNumSeq)
						aAdd( aTemp , {"C6_IDENTB6"	, cNumSeq		, Nil }	) //	Numero do Pedido

					Endif
					aAdd( aTemp , {"AUTDELETA"	, "N"				, Nil }	) //

					cPedDev := QtdDev(QRY->A2_COD , QRY->A2_LOJA , QRY->D1_SERIE , QRY->D1_DOC , QRY->D1_ITEM , nQtde )
					If !Empty(cPedDev)
						cTempMsg 	:= "Esta nota e item ja foi devolvida pelos pedidos : " + cPedDev
						cNumPed 	:= ""
						aAdd( aRet , cNumPed    )
						aAdd( aRet , cTempMsg    )
						FWRestArea(aArea)
						Return aRet

					Endif

					aAdd( aItens , aTemp )

				Endif

			Endif

			QRY->(DbSkip())

		EndDo

		QRY->( DbCloseArea())

		lAutoErrNoFile	:= .F.
		lMsHelpAuto		:= .F.
		lMsErroAuto		:= .F.

		If Len(aCab) > 0 .And. Len(aItens) > 0

			aCab	:= FwVetByDic(aCab	,"SC5")
			MsExecAuto({ |X, Y, Z| MATA410(X, Y, Z)} , aCab , aItens , 3 )

			If lMsErroAuto .And. !( SC5->( DbSeek( xFilial("SC5") + cNumPed ) ) )
				cFileLog 	:= DToS(Date())+"_"+StrTran( Time() , ":", "-")+".log"
				cErro		:= MostraErro( cPathLog, cFileLog )

				// se n capturar mostraerro aparece o help para nao ir informacao faltando
				If Empty(cErro)
					cTempMsg 	:= "Erro: " + Alltrim(FwGetUltHlp()[2][1]) + " Solucao: " + Alltrim(FwGetUltHlp()[3][1])
				Else
					nAt1 		:= At("AJUDA:", cErro)
					nAt2 		:= At("Tabela SC", cErro)-nAt1
					cTempMsg 	:= StrTran(SubStr(cErro, nAt1, nAt2), Chr(13)+Chr(10))

				Endif
				cNumPed := ""

			Endif

		Endif

	Endif

	FWRestArea(aArea)

	aAdd( aRet , cNumPed	)
	aAdd( aRet , cTempMsg	)

Return aRet
// #################################################################################################################################
//	Gera a query conforme os recnos
// #################################################################################################################################
Static Function MontaQry( nRecno , cRecSD1 )

	cQuery	:= " SELECT " + Enter
	cQuery	+= "	DISTINCT " + Enter
	cQuery	+= "	SA2.A2_COD, " + Enter
	cQuery	+= "	SA2.A2_LOJA, " + Enter
	cQuery	+= "	SA2.A2_NATUREZ, " + Enter
	cQuery	+= "	CASE " + Enter
	cQuery	+= "		WHEN SF1.F1_COND = ' ' THEN '001' " + Enter
	cQuery	+= "		ELSE SF1.F1_COND " + Enter
	cQuery	+= "		END AS F1_COND, " + Enter
	cQuery	+= "	SD1.D1_DOC, " + Enter
	cQuery	+= "	SD1.D1_SERIE, " + Enter
	cQuery	+= "	SD1.D1_ITEM, " + Enter
	cQuery	+= "	SD1.D1_COD, " + Enter
	cQuery	+= "	SD1.D1_VALDESC, " + Enter
	cQuery	+= "	SD1.D1_LOCAL, " + Enter
	cQuery	+= "	SD1.D1_QUANT, " + Enter
	cQuery	+= "	SD1.D1_VUNIT, " + Enter
	cQuery	+= "	TESD.F4_CODIGO, " + Enter
	cQuery	+= "	TESD.F4_CF " + Enter
	cQuery	+= " " + Enter
	cQuery	+= " FROM "+RetSqlName("SF1")+" SF1 " + Enter
	cQuery	+= " " + Enter
	cQuery	+= "	INNER JOIN "+RetSqlName("SD1")+" SD1 " + Enter
	cQuery	+= "	ON SD1.D1_FILIAL = SF1.F1_FILIAL " + Enter
	cQuery	+= "		AND SD1.D_E_L_E_T_ = ' ' " + Enter
	cQuery	+= "		AND SD1.D1_DOC = SF1.F1_DOC " + Enter
	cQuery	+= "		AND SD1.D1_SERIE = SF1.F1_SERIE " + Enter
	cQuery	+= "		AND SD1.D1_FORNECE = SF1.F1_FORNECE " + Enter
	cQuery	+= "		AND SD1.D1_LOJA = SF1.F1_LOJA " + Enter
	cQuery	+= "		AND SD1.D1_TIPO = SF1.F1_TIPO " + Enter
	If !Empty(cRecSD1)
		cQuery	+= "		AND SD1.D1_ITEM IN ('" + cRecSD1 + "') " + Enter

	Endif
	cQuery	+= " " + Enter
	cQuery	+= "	INNER JOIN "+RetSqlName("SF4")+" SF4 " + Enter
	cQuery	+= "	ON SF4.F4_FILIAL = '"+xFilial("SF4")+"' " + Enter
	cQuery	+= "		AND SF4.D_E_L_E_T_ = ' ' " + Enter
	cQuery	+= "		AND SF4.F4_CODIGO = SD1.D1_TES " + Enter
	cQuery	+= " " + Enter
	cQuery	+= "	INNER JOIN "+RetSqlName("SF4")+" TESD " + Enter
	cQuery	+= "	ON TESD.F4_FILIAL = SF4.F4_FILIAL " + Enter
	cQuery	+= "		AND TESD.D_E_L_E_T_ = ' ' " + Enter
	cQuery	+= "		AND TESD.F4_CODIGO = SF4.F4_TESDV " + Enter
	cQuery	+= " " + Enter
	cQuery	+= "	INNER JOIN "+RetSqlName("SA2")+" SA2 " + Enter
	cQuery	+= "	ON SA2.A2_FILIAL = '"+xFilial("SA2")+"' " + Enter
	cQuery	+= "		AND SA2.D_E_L_E_T_ = ' ' " + Enter
	cQuery	+= "		AND SA2.A2_COD = SF1.F1_FORNECE " + Enter
	cQuery	+= "		AND SA2.A2_LOJA = SF1.F1_LOJA " + Enter
	cQuery	+= " " + Enter
	cQuery	+= " WHERE SF1.F1_FILIAL = '"+xFilial("SF1")+"' " + Enter
	cQuery	+= "	AND SF1.R_E_C_N_O_ = '"+cValToChar(nRecno)+"' " + Enter
	cQuery	+= "	AND SF1.D_E_L_E_T_ = ' ' " + Enter
	cQuery	+= " " + Enter
	cQuery	+= " ORDER BY SD1.D1_DOC, SD1.D1_SERIE, SD1.D1_ITEM " + Enter

	cQuery	:= ChangeQuery( cQuery )

Return cQuery
// #################################################################################################################################
Static Function fRetCfop( cCodCli , cLoja , cTES , cOpera , cTipo )

	Local aAreaSA1	 := SA1->(GetArea())
	Local aAreaSA2	 := SA2->(GetArea())
	Local aAreaSF4	 := SF4->(GetArea())
	Local aDadosCfo	 := {}
	Local cCFOP		 := ""

	If cTipo == "N"
		// Seta os Indices Utilizados
		SA1->(DbSetOrder(1)) //	A1_FILIAL+A1_COD+A1_LOJA
		SF4->(DbSetOrder(1)) //	F4_FILIAL+F4_CODIGO
		If SA1->( DbSeek( xFilial("SA1") + cCodCli + cLoja , .F. ) )

			If SF4->( DbSeek( xFilial("SF4") + cTES , .F. ) )

				aAdd(aDadosCfo,{"OPERNF"	, cOpera			})
				aAdd(aDadosCfo,{"TPCLIFOR"	, SA1->A1_TIPO		})
				aAdd(aDadosCfo,{"UFDEST"	, SA1->A1_EST		})
				aAdd(aDadosCfo,{"INSCR"		, SA1->A1_INSCR		})
				aAdd(aDadosCfo,{"CONTR"		, SA1->A1_CONTRIB	})
				aAdd(aDadosCfo,{"FRETE"		, "F"				})

				cCFOP	:= MaFisCfo(,SF4->F4_CF,aDadosCfo)

			Endif

		Endif

	Else
		// Seta os Indices Utilizados
		SA2->(DbSetOrder(1)) //	A2_FILIAL + A2_COD + A2_LOJA
		SF4->(DbSetOrder(1)) //	F4_FILIAL + F4_CODIGO
		If SA2->( DbSeek( xFilial("SA2") + cCodCli + cLoja , .F. ) )

			If SF4->( DbSeek( xFilial("SF4") + cTES , .F. ) )

				aAdd(aDadosCfo,{"OPERNF"	, cOpera			})
				aAdd(aDadosCfo,{"TPCLIFOR"	, "R"				})
				aAdd(aDadosCfo,{"UFDEST"	, SA2->A2_EST		})
				aAdd(aDadosCfo,{"INSCR"		, SA2->A2_INSCR		})
				aAdd(aDadosCfo,{"CONTR"		, SA2->A2_CONTRIB	})
				aAdd(aDadosCfo,{"FRETE"		, "F"				})

				cCFOP	:= MaFisCfo(,SF4->F4_CF,aDadosCfo)

			Endif

		Endif

	Endif

	//	Restaura a Ordem e posicionamentos Originais dos Arquivos
	RestArea(aAreaSA1)
	RestArea(aAreaSA2)
	RestArea(aAreaSF4)

Return(cCFOP)
// #################################################################################################################################
Static Function QtdDev( xA2_COD, xA2_LOJA, xD1_SERIE, xD1_DOC, xD1_ITEM , _nQtde )

	Local aArea				:= GetArea()
	Local cPedDev			:= ""
	Local cQuery			:= ""
	Local cAlias			:= ""

	Default xA2_COD			:= ""
	Default xA2_LOJA		:= ""
	Default xD1_SERIE		:= ""
	Default xD1_DOC			:= ""
	Default xD1_ITEM		:= ""
	Default _nQtde			:= 0

	cQuery    := " SELECT " + Enter
	cQuery    += "    LISTAGG(NVL(SC6.C6_NUM,' '),', ') PEDIDOS, " + Enter
	cQuery    += "    SB6.B6_SALDO SALDO " + Enter
	cQuery    += " FROM SD1080 SD1 " + Enter
	cQuery    += " " + Enter
	cQuery    += "    INNER JOIN SB6080 SB6 " + Enter
	cQuery    += "    ON SB6.B6_FILIAL = SD1.D1_FILIAL " + Enter
	cQuery    += "        AND SB6.D_E_L_E_T_ = ' ' " + Enter
	cQuery    += "        AND SB6.B6_SERIE = SD1.D1_SERIE " + Enter
	cQuery    += "        AND SB6.B6_DOC = SD1.D1_DOC " + Enter
	cQuery    += "        AND SB6.B6_CLIFOR = SD1.D1_FORNECE " + Enter
	cQuery    += "        AND SB6.B6_LOJA = SD1.D1_LOJA " + Enter
	cQuery    += "        AND SB6.B6_IDENT = SD1.D1_IDENTB6 " + Enter
	cQuery    += " " + Enter
	cQuery    += "    LEFT JOIN SC6080 SC6 " + Enter
	cQuery    += "    ON SC6.C6_FILIAL = SB6.B6_FILIAL " + Enter
	cQuery    += "        AND SC6.D_E_L_E_T_ = ' ' " + Enter
	cQuery    += "        AND SC6.C6_PRODUTO = SB6.B6_PRODUTO " + Enter
	cQuery    += "        AND SC6.C6_NFORI = SD1.D1_DOC " + Enter
	cQuery    += "        AND SC6.C6_SERIORI = SD1.D1_SERIE " + Enter
	cQuery    += "        AND SC6.C6_ITEMORI = SD1.D1_ITEM " + Enter
	cQuery    += " " + Enter
	cQuery    += " WHERE SD1.D1_FILIAL = '"+xFilial("SD1")+"' " + Enter
	cQuery    += "    AND SD1.D_E_L_E_T_ = ' ' " + Enter
	cQuery    += "    AND SD1.D1_FORNECE = '" + xA2_COD + "' " + Enter
	cQuery    += "    AND SD1.D1_LOJA = '" + xA2_LOJA + "' " + Enter
	cQuery    += "    AND SD1.D1_SERIE = '" + xD1_SERIE + "' " + Enter
	cQuery    += "    AND SD1.D1_DOC = '" + xD1_DOC + "' " + Enter
	cQuery    += "    AND SD1.D1_ITEM = '" + xD1_ITEM + "' " + Enter
	cQuery    += " " + Enter
	cQuery    += " GROUP BY SB6.B6_SALDO " + Enter

	cAlias	:= GetNextAlias()
	cQuery	:= ChangeQuery(cQuery)

	MpSysOpenQuery( cQuery , cAlias )

	If (cAlias)->(!EoF())

		If (cAlias)->SALDO < _nQtde
			cPedDev    := AllTrim( (cAlias)->PEDIDOS )

		EndIf

	EndIf

	(cAlias)->(DbCloseArea())

	RestArea( aArea )

Return cPedDev
// #################################################################################################################################
Static Function GetSeqB6( xA2_COD, xA2_LOJA, xD1_SERIE, xD1_DOC, xD1_ITEM)

	Local aArea		:= GetArea()
	Local cNum 		:= ""
	Local cAlias 	:= ""
	Local cQuery 	:= ""

	cQuery	+= " SELECT SB6.B6_IDENT " + Enter
	cQuery	+= " FROM "+RetSqlName("SD1")+" SD1 " + Enter
	cQuery	+= " " + Enter
	cQuery	+= "	INNER JOIN "+RetSqlName("SB6")+" SB6 " + Enter
	cQuery	+= "	ON SB6.B6_FILIAL = SD1.D1_FILIAL " + Enter
	cQuery	+= "		AND SB6.D_E_L_E_T_ = ' ' " + Enter
	cQuery	+= "		AND SB6.B6_SERIE = SD1.D1_SERIE " + Enter
	cQuery	+= "		AND SB6.B6_DOC = SD1.D1_DOC " + Enter
	cQuery	+= "		AND SB6.B6_CLIFOR = SD1.D1_FORNECE " + Enter
	cQuery	+= "		AND SB6.B6_LOJA = SD1.D1_LOJA " + Enter
	cQuery	+= "		AND SB6.B6_IDENT = SD1.D1_IDENTB6 " + Enter
	cQuery	+= " " + Enter
	cQuery	+= " WHERE SD1.D1_FILIAL = '"+xFilial("SD1")+"' " + Enter
	cQuery	+= "	AND SD1.D_E_L_E_T_ = ' ' " + Enter
	cQuery	+= "	AND SD1.D1_FORNECE = '"+xA2_COD+"' " + Enter
	cQuery	+= "	AND SD1.D1_LOJA = '"+xA2_LOJA+"' " + Enter
	cQuery	+= "	AND SD1.D1_SERIE = '"+xD1_SERIE+"' " + Enter
	cQuery	+= "	AND SD1.D1_DOC = '"+xD1_DOC+"' " + Enter
	cQuery	+= "	AND SD1.D1_ITEM = '"+xD1_ITEM+"' " + Enter

	cAlias	:= GetNextAlias()

	cQuery	:= ChangeQuery(cQuery)

	MpSysOpenQuery(cQuery, cAlias)

	If (cAlias)->(!EoF())
		cNum := AllTrim((cAlias)->(B6_IDENT))

	EndIf

	(cAlias)->(DbCloseArea())

	RestArea( aArea )

Return cNum
