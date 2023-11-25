#' @title Gráfico PACF
#'
#' @import tidyverse
#' @import ggplot2
#' @import ggdark
#'
#' @param vetor vetor da série para o cálculo da ACF (numeric);
#' @param cor cor das bolinhas e das linhas (character), default é preto;
#' @param bolinha gráfico terá os valores marcados por bolinhas ou não, default é sem bolinhas (boleano);
#' @param esp espessura da linha valor (numeric) da espessura da linha, default é 1;
#' @param lagg lag.max do gráfico (integer), default é 10.
#' @param tema tema do gráfico, default é 'claro'.
#'
#' @description
#' Função que retorna um gráfico PACF de uma série temporal passada por parâmetro, com bandas de confiança de 95%.
#'
#' @return
#' Gráfico PACF
#'
#' @export


stv_pacf <- function(vetor, cor = "purple", bolinha = FALSE, linha = 1, lagg = 20, tema = "claro"){
  if(class(vetor) == "numeric" & class(cor) == "character" & class(bolinha) == "logical" &
     class(linha) == "numeric" & class(lagg) == "numeric" & class(tema) == "character"){
    pacf = pacf(vetor, plot = FALSE, lag.max = lagg)
    pacf_dataframe = with(pacf, data.frame(lag, acf))
    limite_superior_pacf = qnorm((1 + (1 - 0.05))/2)/sqrt(pacf$n.used)
    limite_inferior_pacf = -limite_superior_pacf
    if(tema == "claro"){
      if(bolinha == FALSE){
        ggplot(pacf_dataframe, aes(x = lag, y = acf)) +
          geom_hline(yintercept = 0, color = "black") +
          geom_hline(yintercept = limite_superior_pacf, linetype = 2, color = "black") +
          geom_hline(yintercept = limite_inferior_pacf, linetype = 2, color = "black") +
          geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf), color = cor, size = linha) +
          labs(title = "PACF") +
          theme(plot.background = element_rect(fill = "#D3D3D3"),
                panel.background = element_blank(),
                panel.grid.major = element_line(color = "#A9A9A9", size = 0.2),
                panel.grid.minor = element_line(color = "#A9A9A9", size = 0.2),
                axis.ticks = element_blank())
      }else{
        ggplot(pacf_dataframe, aes(x = lag, y = acf)) +
          geom_hline(yintercept = 0, color = "black") +
          geom_hline(yintercept = limite_superior_pacf, linetype = 2, color = "black") +
          geom_hline(yintercept = limite_inferior_pacf, linetype = 2, color = "black") +
          geom_point(size = 3.5, color = cor, fill = alpha(cor, 0.1), alpha = 0.5, shape = 21, stroke = 1.5) +
          geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf), color = cor, size = linha) +
          labs(title = "ACF") +
          theme(plot.background = element_rect(fill = "#D3D3D3"),
                panel.background = element_blank(),
                panel.grid.major = element_line(color = "#A9A9A9", size = 0.2),
                panel.grid.minor = element_line(color = "#A9A9A9", size = 0.2),
                axis.ticks = element_blank())
      }
    }else{
      if(tema == "escuro"){
        if(bolinha == FALSE){
          ggplot(pacf_dataframe, aes(x = lag, y = acf)) +
            geom_hline(yintercept = 0, color = "white") +
            geom_hline(yintercept = limite_superior_pacf, linetype = 2, color = "white") +
            geom_hline(yintercept = limite_inferior_pacf, linetype = 2, color = "white") +
            geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf), color = cor, size = linha) +
            labs(title = "PACF") +
            dark_theme_gray() +
            theme(plot.background = element_rect(fill = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(color = "grey30", size = 0.2),
                  panel.grid.minor = element_line(color = "grey30", size = 0.2),
                  axis.ticks = element_blank())
        }else{
          ggplot(pacf_dataframe, aes(x = lag, y = acf)) +
            geom_hline(yintercept = 0, color = "white") +
            geom_hline(yintercept = limite_superior_pacf, linetype = 2, color = "white") +
            geom_hline(yintercept = limite_inferior_pacf, linetype = 2, color = "white") +
            geom_point(size = 3.5, color = cor, fill = alpha(cor, 0.1), alpha = 0.5, shape = 21, stroke = 1.5) +
            geom_segment(aes(x = lag, xend = lag, y = 0, yend = acf), color = cor, size = linha) +
            labs(title = "PACF") +
            dark_theme_gray() +
            theme(plot.background = element_rect(fill = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(color = "grey30", size = 0.2),
                  panel.grid.minor = element_line(color = "grey30", size = 0.2),
                  axis.ticks = element_blank())
        }
      }else{stop("Erro: tema inválido. O argumento 'tema' precisa estar necessariamente definida como 'claro' ou como 'escuro'.")}
    }
  }else{stop("Erro: Classe de um ou mais argumentos incorretos.")}
}
