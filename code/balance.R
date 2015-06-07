rm(list = ls())
source("functions.R")
source("_function_plotmeans.R") # modified plotmeans
source("_function_summarySE.R") # summary sample means and SE
source("_function_multiplot.R")
packs <- c("foreign", "plyr", "dplyr", "ggplot2", "Matching")
f_install_and_load(packs)

# ---- Load data ----
d_pci <- read.dta("../clean_data/pci_panel.dta")
d_pci_lab <- f_stata_to_df(d_pci)
d_pci_env <- f_stata_to_env(d_pci)

# ---- Check provinces with "negative" corruption ----
d_pci %>%
  group_by(pci_id, year, treatment) %>% summarise(m = mean(reg_corrupt, na.rm=TRUE)) %>%
  group_by(pci_id, year) %>% mutate(d = c(NA, diff(m))) %>%
  print(n = 1000)

d_corr <- d_pci %>%
  group_by(pci_id, treatment) %>% summarise(m = mean(reg_corrupt, na.rm=TRUE)) %>%
  group_by(pci_id) %>% mutate(d = c(NA, diff(m))) %>%
  print(n = 1000)

c_weirdprov <- d_corr[d_corr$d < 0 & !is.na(d_corr$d), ] %>% .[["pci_id"]]

f_corrupt_plot <- function(provname=NULL, textpos=0.5, fdi_only=FALSE) {
  if (!is.null(provname)) {
    d_treatment <- filter(d_pci, treatment == 1, pci_id == provname)
    d_control <- filter(d_pci, treatment == 0, pci_id == provname)
  } else {
    d_treatment <- filter(d_pci, treatment == 1)
    d_control <- filter(d_pci, treatment == 0)
  }

  if (fdi_only==TRUE) {
    d_treatment <- filter(d_treatment, FDI==1)
    d_control <- filter(d_control, FDI==1)
  }

  give.n <- function(x) {
    return(c(y=textpos, label=length(na.omit(x))))
  }

  ggplot() +
    stat_summary(aes(factor(year), reg_corrupt, col="control"), data=d_control, fun.data="mean_cl_boot") +
    stat_summary(aes(factor(year), reg_corrupt, col="treatment"), data=d_treatment, fun.data="mean_cl_boot") +
    stat_summary(aes("all sample", reg_corrupt, col="control"), data=d_control, fun.data="mean_cl_boot") +
    stat_summary(aes("all sample", reg_corrupt, col="treatment"), data=d_treatment, fun.data="mean_cl_boot") +
    stat_summary(aes(factor(year), reg_corrupt, col="control"), data=d_control, geom="text", fun.data=give.n, hjust=0) +
    stat_summary(aes(factor(year), reg_corrupt, col="treatment"), data=d_treatment, geom="text", fun.data=give.n, hjust=1) +
    stat_summary(aes("all sample", reg_corrupt, col="control"), data=d_control, geom="text", fun.data=give.n, hjust=0) +
    stat_summary(aes("all sample", reg_corrupt, col="treatment"), data=d_treatment, geom="text", fun.data=give.n, hjust=1) +
    ylim(c(0, 3)) +
    labs(title=provname, x="Year")
}

plot_list <- list()
for (i in seq_along(c_weirdprov)) {
  plot_list[[i]] <- f_corrupt_plot(c_weirdprov[[i]], textpos = 0)
}
pdf("../figure/provinces_with_negative_listexp.pdf", w=16, h=12)
multiplot(plotlist = plot_list, cols=3)
dev.off()


c_firmbal_vars <- c("est_year", "reg_year", "ownership_type",
                    "manufacturing", "construction", "services", "agriculture", "mining",
                    "equity_est", "equity_thisyear",
                    "labor_est", "labor_thisyear",
                    "performance",
                    "connection", "connection_dich")
fm_balance <- as.formula(paste("treatment ~", paste(c_firmbal_vars, collapse=" + ")))
m_bal <- MatchBalance(fm_balance, data=d_pci)$BeforeMatching
f_create_balancetable(df = d_pci, balance_vars = c_firmbal_vars, bal_result = m_bal)

m_bal_BRVT <- MatchBalance(fm_balance, data=filter(d_pci, pci_id=="BRVT"))$BeforeMatching
f_create_balancetable(d_pci, c_firmbal_vars, m_bal_BRVT)

m_bal_BinhDinh <- MatchBalance(fm_balance, data=filter(d_pci, pci_id=="Binh Dinh"))$BeforeMatching
f_create_balancetable(d_pci, c_firmbal_vars, m_bal_BinhDinh)

m_bal_ThanhHoa <- MatchBalance(fm_balance, data=filter(d_pci, pci_id=="Thanh Hoa"))$BeforeMatching
f_create_balancetable(d_pci, c_firmbal_vars, m_bal_ThanhHoa)

# ---- Overlap between sectors ----

d_pci %>% group_by(isic_rev4_2digitcode, treatment) %>%
  summarise(m = mean(reg_corrupt, na.rm=TRUE)) %>%
  group_by(isic_rev4_2digitcode) %>% mutate(d = c(NA, diff(m)))

d_pci %>% filter(FDI == 1) %>%
  group_by(restrict, treatment) %>% summarise(n = n(), m = mean(reg_corrupt, na.rm=TRUE)) %>%
  group_by(restrict) %>% mutate(d = c(NA, diff(m)))


d_pci$restrict[is.na(d_pci$restrict)] <- "Unknown"
d_pci$connection_dich <- factor(d_pci$connection_dich)
d_pci$performance[d_pci$performance > quantile(d_pci$performance, 0.99, na.rm=TRUE)] <- NA
d_pci$performance[d_pci$performance < quantile(d_pci$performance, 0.01, na.rm=TRUE)] <- NA
f_plot_dist <- function(varname, varlabel) {
  ggplot(data=d_pci) +
    stat_ecdf(aes_string(x=varname, col="restrict", group="restrict")) +
    scale_color_discrete("Restricted industries") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    labs(x = varlabel)
}
c_overlap_vars <- c("ownership_type", "equity_est", "labor_est")
c_overlap_vars_lab <- c("ownership type", "equity at establishment", "labor at establishment")
plotlist <- list()
for (i in seq_along(c_overlap_vars)) {
  plotlist[[i]] <- f_plot_dist(c_overlap_vars[[i]], c_overlap_vars_lab[[i]])
}
pdf("../figure/by_restrict_owner-labor-equity.pdf", w=8, h=6)
multiplot(plotlist = plotlist, cols=2)
dev.off()

c_overlap_vars <- c("performance", "connection")
c_overlap_vars_lab <- c("% change in this year profit", "number of CEO connections with gov")
plotlist <- list()
for (i in seq_along(c_overlap_vars)) {
  plotlist[[i]] <- f_plot_dist(c_overlap_vars[[i]], c_overlap_vars_lab[[i]])
}
pdf("../figure/by_restrict_performance-connection.pdf", w=9, h=3)
multiplot(plotlist = plotlist, cols=2)
dev.off()