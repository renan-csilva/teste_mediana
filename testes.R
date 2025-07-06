
dados_enem <- read.csv("enem.csv", sep = ";")

M <- median(dados_enem$media_matematica)

dados_enem <- dados_enem %>%
  mutate(
    acima_M <- ifelse(media_matematica > mediana, 1, 0),
    igual_M <- ifelse(media_matematica == mediana, 1, 0)
  )

tabela <- dados_enem %>%
  group_by(tipo_escola) %>%
  summarise(
    acima = sum(`acima_M <- ifelse(media_matematica > mediana, 1, 0)`),
    abaixo = n() - acima - sum(`igual_M <- ifelse(media_matematica == mediana, 1, 0)`), # Ignoramos Y = Med
    total = n()
  )

print(tabela)

# EMV sob H_1
p_hat_particular <- tabela$acima[1] / tabela$total[1]
p_hat_publica <- tabela$acima[2] / tabela$total[2]

# EMV sob H_0
p_hat_geral <- sum(tabela$acima) / sum(tabela$total)


# 
# Log-verossimilhança sob H0
log_L0 <- sum(tabela$acima) * log(p_hat_geral) + 
  (sum(tabela$total) - sum(tabela$acima)) * log(1 - p_hat_geral)

# Log-verossimilhança sob H1
log_L1 <- tabela$acima[1] * log(p_hat_particular) + 
  (tabela$total[1] - tabela$acima[1]) * log(1 - p_hat_particular) +
  tabela$acima[2] * log(p_hat_publica) + 
  (tabela$total[2] - tabela$acima[2]) * log(1 - p_hat_publica)

Lambda <- -2 * (log_L0 - log_L1)
p_valor <- pchisq(Lambda, df = 1, lower.tail = FALSE)  # df = 1 (pois comparamos 2 proporções)

print(paste("Estatística TRV =", round(Lambda, 4), "p-valor =", round(p_valor, 4)))


teste_chi2 <- chisq.test(matrix(c(tabela$acima, tabela$abaixo), nrow = 2), correct = FALSE)
print(teste_chi2)

gamma_hat <- 1 / nrow(dados_enem)  # Estimativa pontual
ic_gamma <- binom.test(1, nrow(dados_enem))$conf.int
ic_gamma

#################################
####################################

# Mediana global
M <- median(dados_enem$media_matematica)

# Função segura
log_verossimilhanca <- function(params, dados, grupo) {
  p <- params[1]
  gamma <- ifelse(grupo == "Pública", params[2], 0)  # Gamma só para públicas
  
  if (p <= 1e-10 | (p + gamma) >= (1 - 1e-10)) return(-1e10)
  
  y <- dados$media_matematica[dados$tipo_escola == grupo]
  a <- sum(y > M)
  d <- sum(abs(y - M) < 1e-10)
  c <- sum(y < M)
  
  a * log(p) + d * log(gamma + 1e-10) + c * log(1 - p - gamma + 1e-10)
}

# Otimização
opt_particular <- optim(
  par = c(p = 0.5),  # Só estima p para particulares
  fn = log_verossimilhanca,
  dados = dados_enem,
  grupo = "Particular",
  method = "Brent",  # Método para 1D
  lower = 1e-10,
  upper = 1 - 1e-10
)

opt_publica <- optim(
  par = c(p = 0.5, gamma = 0.1),
  fn = log_verossimilhanca,
  dados = dados_enem,
  grupo = "Pública",
  method = "L-BFGS-B",
  lower = c(1e-10, 1e-10),
  upper = c(1 - 1e-10, 1 - 1e-10)
)


opt_particular
