# Processo Poisson para gerar o tempo de chegada dos clientes
proc_pois <- function(lambda) {
    t_n <- rexp(10000, lambda)
    s_n <- cumsum(t_n) # arrival times

    (s_n)
}

# Função para gerar um vetor indicando a quantidade de clientes que chegaram, por hora, até um tempo final
qntd_clientes_por_hora <- function(tempo_final, qntd_total_clientes) {
    qtd_cliente_hora <- c()
    for (t in 1:tempo_final) {
        qtd_cliente_hora <- append(
            qtd_cliente_hora,
            sum(as.integer(qntd_total_clientes < t & qntd_total_clientes > (t - 1)))
        )
    }
    (qtd_cliente_hora)
}


# Quantidade de itens comprados por cliente na hora t
qntd_itens_cliente <- function(qtd_clientes_nessa_hora) {
    if (qtd_clientes_nessa_hora == 0) {
        (0)
    } else {
        qntd_pedida <- 0
        for (i in 1:qtd_clientes_nessa_hora) {
            qntd_cliente_i <- rpois(1, lambda = 3)
            qntd_pedida <- qntd_pedida + qntd_cliente_i
        }
        (qntd_pedida)
    }
}



# Calcula o estoque final no tempo t, retorna: Quantidade de itens no estoque, quantidade de itens vendidos aos clientes, quantidade de itens que não foram vendidos (prejuizo em itens), retorna informações sobre a reposição do estoque: qntd_itens_para_repor_estoque, tempo_ate_reposicao_estoque
calcular_estoque <- function(estoque_atual, qntd_itens_pedidos_clientes, info_reposicao_estoque) {
    if (info_reposicao_estoque[[2]] > 0) {
        info_reposicao_estoque[[2]] <- info_reposicao_estoque[[2]] - 1
    }

    if (info_reposicao_estoque[[2]] == 0) {
        estoque_atual <- estoque_atual + info_reposicao_estoque[[1]]
        info_reposicao_estoque[[1]] <- 0
    }

    estoque <- estoque_atual - qntd_itens_pedidos_clientes

    if ((estoque <= limite_para_repor_estoque) && (info_reposicao_estoque[[2]] == 0)) {
        info_reposicao_estoque[[1]] <- fazer_reposicao_estoque(estoque)
        info_reposicao_estoque[[2]] <- tempo_ate_reposicao_estoque
    }

    if (estoque < 0) {
        prejuizo_em_itens <- estoque
        estoque <- 0
    }

    qntd_itens_vendidos <- qntd_itens_pedidos_clientes + prejuizo_em_itens

    (list(estoque, qntd_itens_vendidos, info_reposicao_estoque))
}


# Quantidade a repor no estoque
fazer_reposicao_estoque <- function(qntd_itens_no_estoque) {
    reposicao <- estoque_cheio - qntd_itens_no_estoque

    (reposicao)
}

# Custo para repor x unidades de produto no estoque
custo_repor_estoque <- function(qntd_itens_para_repor) {
    custo <- qntd_itens_para_repor * z
    (custo)
}

# Prejuizo total na hora t
prejuizo_total <- function(custo_do_pedido, estoque_atual) {
    (custo_do_pedido + estoque_atual * h)
}

# Receita na hora t
receita <- function(itens_comprados_nessa_hora) {
    (itens_comprados_nessa_hora * r)
}

# Lucro na hora t
lucro <- function(receita, prejuizo_total) {
    (receita - prejuizo_total)
}


# _____________________________________________________________

# ---------------- Variaveis do Sistema --------------
estoque_cheio <- 100
limite_para_repor_estoque <- 50
tempo_final <- 100
tempo_ate_reposicao_estoque <- 3
h <- 0.5 # custo para manter os itens no estoque
z <- 10 # custo que a loja paga por produto para repor estoque
r <- 50 # custo que o cliente paga por produto
prejuizo_em_itens <- 0

info_reposicao_estoque <- c(0, 0)

estoque <- list(estoque_cheio, 0, info_reposicao_estoque)

qntd_total_clientes <- proc_pois(3)

total_clientes_por_hora <- qntd_clientes_por_hora(tempo_final, qntd_total_clientes)

# -------------------- Vetores --------------------
v_pedidos <- c()
v_qtd_vendida <- c()
v_estoque <- c()
v_custo_pedido <- c()
v_prejuizo <- c()
v_receita <- c()
v_lucro <- c()

# -------------------- Simulando --------------------

for (t in 1:tempo_final) {
    print(paste("Tempo:", t, sep = " "))

    print(paste("Quantidade de itens no estoque:", estoque[[1]], sep = " "))

    print(paste(total_clientes_por_hora[t], "clientes chegaram nessa hora", sep = " "))

    qntd_itens_pedidos_clientes <- qntd_itens_cliente(total_clientes_por_hora[t])

    print(paste("Quantidade de total de itens pedidos:", qntd_itens_pedidos_clientes, sep = " "))

    estoque <- calcular_estoque(estoque[[1]], qntd_itens_pedidos_clientes, estoque[[3]])

    print(paste("Quantidade de itens restantes no fim da hora:", estoque[[1]], sep = " "))

    print(paste("Quantidade de itens vendidos aos clientes:", estoque[[2]], sep = " "))

    print(paste("Informações sobre a reposiçao do estoque: ", estoque[3], sep = " "))

    v_pedidos <- append(v_pedidos, ifelse(estoque[[3]][[2]] == tempo_ate_reposicao_estoque, estoque[[3]][[1]], 0))

    v_qtd_vendida <- append(v_qtd_vendida, estoque[[2]])

    v_estoque <- append(v_estoque, estoque[[1]])

    v_custo_pedido <- append(v_custo_pedido, ifelse(estoque[[3]][[2]] == tempo_ate_reposicao_estoque, custo_repor_estoque(estoque[[3]][[1]]), 0))

    v_prejuizo <- append(v_prejuizo, prejuizo_total(v_custo_pedido[t], v_estoque[t]))

    v_receita <- append(v_receita, receita(v_qtd_vendida[t]))

    v_lucro <- append(v_lucro, lucro(v_receita[t], v_prejuizo[t]))

    print("---------------------__________")
}

v_qtd_vendida

v_estoque

v_pedidos

v_custo_pedido

v_prejuizo

v_receita

v_lucro

#---------------------------------------------

# Pacotes para construir os gráficos
library(ggplot2)
library(tidyr)
library(dplyr)

# Variável tempo, do eixo x
t <- 1:tempo_final

df_simulacao <- data.frame(
    Tempo = t,
    Pedido = v_pedidos,
    Vendido = v_qtd_vendida,
    Estoque = v_estoque,
    CustoPedido = v_custo_pedido,
    Prejuizo = v_prejuizo,
    Receita = v_receita,
    Lucro = v_lucro
)

ggplot(df_simulacao, aes(x = Tempo, y = Pedido)) +
    geom_line(color = "royalblue", size = 1) +
    geom_point(color = "black") +
    labs(title = "Pedidos ao Distribuidor", y = "Quantidade Pedida") +
    theme_minimal()

ggplot(df_simulacao, aes(x = Tempo, y = Vendido)) +
    geom_line(color = "royalblue", size = 1) +
    geom_point(color = "black") +
    labs(title = "Itens Vendidos por Hora", y = "Quantidade Vendida") +
    theme_minimal()

ggplot(df_simulacao, aes(x = Tempo, y = Estoque)) +
    geom_line(color = "royalblue", size = 1) +
    geom_point(color = "black") +
    geom_hline(yintercept = limite_para_repor_estoque, linetype = "dashed", color = "red", size = 1) +
    labs(
        title = "Nível de Estoque",
        y = "Itens em Estoque"
    ) +
    theme_minimal()


ggplot(df_simulacao, aes(x = Tempo, y = CustoPedido)) +
    geom_line(color = "royalblue", size = 1) +
    geom_point(color = "black") +
    labs(title = "Custo com Pedidos", y = "Custo (R$)") +
    theme_minimal()

ggplot(df_simulacao, aes(x = Tempo, y = Prejuizo)) +
    geom_line(color = "royalblue", size = 1) +
    geom_point(color = "black") +
    labs(title = "Prejuízo por Hora", y = "Prejuízo (R$)") +
    theme_minimal()

ggplot(df_simulacao, aes(x = Tempo, y = Receita)) +
    geom_line(color = "royalblue", size = 1) +
    geom_point(color = "black") +
    labs(title = "Receita por Hora", y = "Receita (R$)") +
    theme_minimal()

ggplot(df_simulacao, aes(x = Tempo, y = Lucro)) +
    geom_line(color = "royalblue", size = 1) +
    geom_point(color = "black") +
    labs(title = "Lucro por Hora", y = "Lucro (R$)") +
    theme_minimal()

df_plot <- df_simulacao %>%
    select(Tempo, Prejuizo, Receita, Lucro) %>%
    pivot_longer(cols = -Tempo, names_to = "Variavel", values_to = "Valor")

ggplot(df_plot, aes(x = Tempo, y = Valor, color = Variavel, linetype = Variavel, size = Variavel)) +
    geom_line() +
    labs(
        title = "Prejuízo, Receita e Lucro ao Longo do Tempo",
        y = "Valor (R$)",
        color = "Indicador"
    ) +
    theme_minimal() +
    scale_color_manual(values = c(
        "Prejuizo" = "red",
        "Receita" = "darkgreen",
        "Lucro" = "black"
    )) +
    scale_linetype_manual(values = c(
        "Prejuizo" = "dashed",
        "Receita" = "dashed",
        "Lucro" = "solid"
    )) +
    scale_size_manual(values = c(
        "Prejuizo" = 0.8, # Menor espessura para Prejuízo
        "Receita" = 0.8, # Menor espessura para Receita
        "Lucro" = 1.5 # Maior espessura para Lucro
    )) +
    guides(
        size = "none", # Remove legenda de espessura
        linetype = guide_legend(override.aes = list(size = 1)) # Ajusta a legenda de tipo de linha
    )


Lucro_total <- sum(v_lucro)
Lucro_medio_por_hora <- mean(v_lucro)
