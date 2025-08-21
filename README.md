# WSGERANF - WebService para Geração de Notas Fiscais

## Descrição
WebService REST desenvolvido em AdvPL para o sistema Protheus que automatiza a geração de Notas Fiscais eletrônicas (NFe) com suporte a pedidos normais e devolução de compras.

## Endpoints

### 1. GET / - Home
- **Descrição**: Retorna informações da versão da API e dados de produção
- **Método**: GET
- **URL**: `/`
- **Resposta**: JSON com informações da versão e data/hora

### 2. POST /GERANF - Geração de NF
- **Descrição**: Gera uma nova Nota Fiscal eletrônica
- **Método**: POST
- **URL**: `/GERANF`
- **Content-Type**: application/json

#### Campos obrigatórios:
```json
{
  "codigo_fornecedor": "string",    // Código do cliente/fornecedor
  "loja": "string",                 // Loja do cliente/fornecedor
  "codigo_transportadora": "string", // Código da transportadora
  "numero_nf_devolvida": "string",  // Número da NF para devolução (opcional)
  "condicao_pagamento": "string",   // Condição de pagamento
  "numero_solicitacao": "string",   // Número da solicitação
  "solicitante": "string",          // Nome do solicitante
  "tipo_volume": "string",          // Tipo do volume
  "quantidade_volume": "number",    // Quantidade de volumes
  "peso_kg": "number",              // Peso em KG
  "tipo_pedido": "string",          // Tipo do pedido (N=Normal, D=Devolução, U=Fornecedor)
  "produtos": [
    {
      "codigo_produto": "string",    // Código do produto
      "quantidade_produto": "number", // Quantidade
      "valor_produto": "number",     // Valor unitário
      "almoxarifado": "string",      // Código do almoxarifado
      "item": "string",              // Item da NF original (para devolução)
      "tes": "string"                // Código do TES
    }
  ]
}
```

#### Campos opcionais:
- `codcliente`: Alternativa ao `codigo_fornecedor`
- `cod_rv`: Código da unidade destino ou pedido de compra
- `observacao`: Observações adicionais
- `codigo_destino`: Código de destino do produto

#### Resposta de sucesso:
```json
{
  "pedido": "string",              // Número do pedido gerado
  "nf": "string",                  // Número da NF
  "serie": "string",               // Série da NF
  "chave": "string",               // Chave de acesso da NFe
  "arquivo_pdf": "string",         // Caminho do arquivo PDF
  "arquivo": "string",             // Arquivo PDF em base64
  "error": ""                      // Vazio em caso de sucesso
}
```

#### Resposta de erro:
```json
{
  "status": "Erro",
  "error": "string",               // Descrição do erro
  "pedido": "",
  "nf": "",
  "serie": "",
  "chave": "",
  "arquivo_pdf": "",
  "arquivo": ""
}
```

## Funcionalidades

### Processo de Geração Normal:
1. **Validação**: Verificação dos dados de entrada e campos obrigatórios
2. **Criação do Pedido**: Geração automática do pedido de venda/compra
3. **Liberação**: Liberação automática do pedido para faturamento
4. **Geração NFe**: Criação do documento de saída
5. **Transmissão SEFAZ**: Envio para aprovação na SEFAZ
6. **Monitoramento**: Verificação do status até aprovação
7. **Geração DANFE**: Criação do arquivo PDF
8. **Retorno**: Arquivo PDF codificado em base64

### Processo de Devolução:
1. **Validação**: Verificação da NF original e fornecedor
2. **Localização**: Busca da NF original no sistema
3. **Criação Pedido**: Geração do pedido de devolução
4. **Processo NFe**: Mesmo fluxo da geração normal
5. **Vinculação**: Ligação com a NF original através do B6_IDENT

## Parâmetros do Sistema

| Parâmetro | Descrição | Padrão |
|-----------|-----------|---------|
| `XX_GNFS036` | Pasta para arquivos DANFE | `"WSGERANF\gera_nf\danfe"` |
| `XX_GNFS037` | Pasta temporária | `"WSGERANF\gera_nf\tmp"` |
| `XX_GNFS038` | Tempo espera status NFe (ms) | `1500` |
| `XX_GNFS039` | Tentativas busca status | `3` |
| `XX_GNFS03A` | Códigos status SEFAZ válidos | `"100/030/001/102"` |
| `XX_GNFSLOG` | Ativa log detalhado | `.F.` |
| `XX_INDSB6` | Usa índice SB6 personalizado | `.F.` |
| `MV_SPEDURL` | URL do serviço TSS/SPED | - |
| `MV_ESPECIE` | Configuração séries NFe | - |

## Validações Implementadas

### Campos Obrigatórios:
- Código fornecedor OU código cliente
- Todos os campos marcados como obrigatórios no array `aCamposObrt`
- Produtos com códigos, quantidades e valores válidos
- TES e almoxarifados existentes no cadastro

### Cadastros:
- **Clientes/Fornecedores**: Validação de existência e desbloqueio
- **Produtos**: Verificação no cadastro SB1
- **TES**: Validação no cadastro SF4  
- **Condições de Pagamento**: Verificação no SE4
- **Transportadoras**: Validação se informado

### SEFAZ:
- Configuração TSS ativa
- Ambiente, versão e modalidade configurados
- Status de retorno dentro dos códigos válidos
- Geração de chave de acesso

## Tipos de Pedido Suportados

| Tipo | Descrição | Observações |
|------|-----------|-------------|
| `N` | Normal (Cliente) | Venda padrão para cliente |
| `D` | Devolução de Compra | Retorno de mercadoria para fornecedor |
| `U` | Fornecedor | Pedido tipo "B" no sistema |

## Tratamento de Erros

### Erros de Validação:
- Campos obrigatórios não preenchidos
- Cadastros inexistentes ou bloqueados
- Dados inválidos ou inconsistentes

### Erros de Processamento:
- Falha na criação do pedido (com log detalhado)
- Problemas na liberação do estoque
- Erro na geração da NFe

### Erros SEFAZ:
- Falha na transmissão
- Rejeição pela SEFAZ com código e mensagem
- Timeout na consulta de status

### Erros de Arquivo:
- Problemas na geração do PDF
- Falha na codificação base64
- Problemas de acesso às pastas

## Logs e Debugging

### Console Logs:
Todas as operações são registradas com prefixo `"GERA NF -"` incluindo:
- Início e fim de cada etapa
- Números de pedidos e NFes gerados
- Caminhos de arquivos criados
- Erros detalhados com contexto

### Log Detalhado (Parâmetro XX_GNFSLOG):
Quando ativado, grava logs completos usando a função `U_kGeraCV8`:
- Dados de entrada (JSON sanitizado)
- Dados de saída (com arquivo removido por segurança)
- Timestamps de início e fim

## Dependências

### Sistema:
- **Protheus 12.1.25+** com módulos de faturamento
- **TSS (TOTVS Services SPED)** configurado e ativo
- **Ambiente SEFAZ** configurado (homologação/produção)

### Cadastros Básicos:
- Empresas e filiais configuradas
- Clientes/Fornecedores cadastrados
- Produtos com códigos corretos  
- TES de entrada e saída configurados
- Condições de pagamento ativas
- Transportadoras (se utilizado)

### Funções Customizadas:
- `U_WSHOME()`: Retorna dados básicos da API
- `U_kGeraCV8()`: Função de log customizada  
- `U_Console()`: Função de log no console
- `U_DANFEProc()`: Processamento customizado do DANFE
- `U_fPedDev()`: Função para pedidos de devolução

## Estrutura de Arquivos

```
\WSGERANF\
├── gera_nf\
│   ├── danfe\          # Arquivos PDF finais
│   └── tmp\            # Arquivos temporários
└── Logs\
    ├── Pedido\         # Logs de criação de pedidos
    └── Devolucao\      # Logs de devolução
```

## Observações Técnicas

### Integração:
- Compatível com padrões REST
- Retorno sempre em JSON
