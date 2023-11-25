#' @title Gráfico Sequencial
#'
#' @import tidyverse
#' @import ggplot2
#' @import ggdark
#' @import gganimate
#' @import gifski
#'
#' @param dados objeto do tipo data.frame;
#' @param x nome da coluna no data frame dos dados do eixo x
#' @param y nome da coluna no data frame dos dados do eixo y
#' @param eixo_x denominacao no eixo x, default é o nome da coluna informado no parametro x
#' @param eixo_y denominacao no eixo y, default é o nome da coluna informado no parametro y
#' @param t?tulo objeto do tipo character, default = "Grafico de Sequencia";
#' @param bolinha gr?fico ter? as observa??es marcadas por bolinhas ou n?o, default ? sem bolinha (boleano);
#' @param cor cor da linha e das bolinhas (character), default ? preto;
#' @param tema tema do gráfico, default = claro
#' @param anim se o grafico sera animado ou nao (boleano).
#'
#' @description
#' Função que retorna um gráfico ACF de uma série temporal passada por parâmetro
#'
#' @return
#' Gráfico Sequencial
#'
#' @export

stv_seq <- function(dados, x, y, eixo_x = x, eixo_y = y, titulo = "Gráfico de Sequência", bolinha = FALSE, cor = "purple", tema = "claro", anim = F){
  if(class(dados) == "data.frame" & class(x) == "character" & class(y) == "character" &
     class(eixo_x) == "character" & class(eixo_y) == "character" & class(titulo) == "character" &
     class(bolinha) == "logical" & class(cor) == "character" & class(tema) == "character"){
    if(tema == "claro"){
      if(bolinha == TRUE){
        resultado <- ggplot(dados, aes(x = .data[[x]], y = .data[[y]])) + geom_line(color = cor, size = 1) +
          labs(title = titulo, x = eixo_x, y = eixo_y) + geom_point(color = cor) +
          theme(plot.background = element_rect(fill = "#D3D3D3"),
                panel.background = element_blank(),
                panel.grid.major = element_line(color = "#A9A9A9", size = 0.2),
                panel.grid.minor = element_line(color = "#A9A9A9", size = 0.2),
                axis.ticks = element_blank())
      }else{
        resultado <- ggplot(dados, aes(x = .data[[x]], y = .data[[y]])) + geom_line(color = cor, size = 1) +
          labs(title = titulo, x = eixo_x, y = eixo_y) +
          theme(plot.background = element_rect(fill = "#D3D3D3"),
                panel.background = element_blank(),
                panel.grid.major = element_line(color = "#A9A9A9", size = 0.2),
                panel.grid.minor = element_line(color = "#A9A9A9", size = 0.2),
                axis.ticks = element_blank())

      }
    }else{
      if(tema == "escuro"){
        if(bolinha == TRUE){
          resultado <- ggplot(dados, aes(x = .data[[x]], y = .data[[y]])) + geom_line(color = cor, size = 1) +
            labs(title = titulo, x = eixo_x, y = eixo_y) + geom_point(color = cor) +
            dark_theme_gray() +
            theme(plot.background = element_rect(fill = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(color = "grey30", size = 0.2),
                  panel.grid.minor = element_line(color = "grey30", size = 0.2),
                  axis.ticks = element_blank())
        }else{
          resultado <- ggplot(dados, aes(x = .data[[x]], y = .data[[y]])) + geom_line(color = cor, size = 1) +
            labs(title = titulo, x = eixo_x, y = eixo_y) +
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
  if (anim == T){
    resultado + transition_reveal(.data[[x]])
  }
  else{
    resultado
  }
}
