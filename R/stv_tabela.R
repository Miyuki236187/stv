#' @title Tabela das principais estatísticas
#'
#' @param dados dados da série temporal
#'
#' @description
#' Função que retorna uma tabela com as principais estatísticas da série temporal
#'
#' @return
#' Data Frame
#'
#' @export

stv_tabela = function(dados){
  max = max(dados)
  min = min(dados)
  sd = sd(dados)
  median = median(dados)

  df = data.frame(Estatistica = c("Mínimo", "Máximo", "Desvio Padrão", "Mediana"),
                  Valor = c(min, max, sd, median))
  return(df)
}

