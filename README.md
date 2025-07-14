# WSGERANF - WebService para Geração de Notas Fiscais

## Descrição
WebService REST desenvolvido em AdvPL para o sistema Protheus que automatiza a geração de Notas Fiscais eletrônicas (NFe).

## Endpoints

### 1. GET / - Home
- **Descrição**: Retorna informações da versão da API
- **Método**: GET
- **URL**: `/`
- **Resposta**: JSON com informações da versão e data/hora

### 2. POST /DANFE - Geração de NF
- **Descrição**: Gera uma nova Nota Fiscal eletrônica
- **Método**: POST
- **URL**: `/DANFE`
- **Content-Type**: application/json

#### Campos obrigatórios:
```json
{
  "codigo_fornecedor": "string",
  "loja": "string", 
  "codigo_transportadora": "string",
  "condicao_pagamento": "string",
  "numero_solicitacao": "string",
  "solicitante": "string",
  "tipo_volume": "string",
  "quantidade_volume": "number",
  "peso_kg": "number",
  "tipo_saida": "string",
  "cod_rv": "string",
  "produtos": [
    {
      "codigo_produto": "string",
      "quantidade_produto": "number", 
      "valor_produto": "number",
      "almoxarifado": "string",
      "tes": "string"
    }
  ]
}
```

#### Campos opcionais:
- `numero_nf_devolvida`: Número da NF para devolução

#### Resposta:
```json
{
  "pedido": "string",
  "nf": "string", 
  "serie": "string",
  "chave": "string",
  "arquivo_pdf": "string",
  "arquivo": "string (base64)",
  "error": "string"
}
```

### 3. POST /RETORNO - Retorno e Devolução
- **Descrição**: Processa retorno/devolução de NF existente
- **Método**: POST
- **URL**: `/RETORNO`
- **Content-Type**: application/json

#### Campos obrigatórios:
```json
{
  "pedido": "string"
}
```

#### Resposta:
```json
{
  "pedido": "string",
  "nf": "string",
  "serie": "string", 
  "chave": "string",
  "arquivo_pdf": "string",
  "arquivo": "string (base64)",
  "error": "string"
}
```

## Funcionalidades

### Processo de Geração (/DANFE):
1. Validação dos dados de entrada
2. Criação do pedido de venda
3. Liberação do pedido
4. Geração da Nota Fiscal eletrônica
5. Transmissão para SEFAZ
6. Geração do arquivo PDF (DANFE)
7. Retorno do arquivo em base64

### Processo de Retorno (/RETORNO):
1. Localização do pedido existente
2. Liberação do pedido
3. Geração da NFe de retorno/devolução
4. Geração do PDF
5. Retorno do arquivo em base64

## Parâmetros do Sistema

| Parâmetro | Descrição | Padrão |
|-----------|-----------|---------|
| `ZZ_GRNF036` | Pasta para arquivos DANFE | `"geranf\gera_nf\danfe"` |
| `ZZ_GRNF037` | Pasta temporária DANFE | `"geranf\gera_nf\danfe_tmp"` |
| `ZZ_GRNF038` | Tempo espera status NFe (ms) | `1500` |
| `ZZ_GRNF039` | Tentativas busca status | `3` |
| `ZZ_GRNF03A` | Códigos status SEFAZ válidos | `"100/030/001/102"` |
| `MV_SPEDURL` | URL do serviço SPED | - |
| `MV_ESPECIE` | Configuração séries NFe | - |

## Dependências
- Sistema Protheus
- TSS (TOTVS Services SPED)
- Configuração SEFAZ ativa
- Cadastros básicos (clientes, produtos, TES, etc.)

## Tratamento de Erros
O sistema retorna erros detalhados para:
- Campos obrigatórios não preenchidos
- Cadastros não encontrados
- Falhas na transmissão SEFAZ
- Problemas na geração de arquivos

## Logs
Todas as operações são registradas no console com prefixo `WSGERANF` para facilitar o debugging.

## Observações
- O arquivo PDF é retornado em formato base64 no campo `arquivo`
- O sistema valida automaticamente se os cadastros necessários existem
- A numeração da NF é controlada automaticamente pelo sistema
- Suporte a diferentes tipos de saída (R - Retorno, N - Normal)
