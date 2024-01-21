setwd("~/RColSim_v1/output/")
run_type <- "supply_and_demand"
plot_dir <- paste0(getwd(), "/plots/", run_type, "/")
library(xts)

seconds_in_month = 86400 * c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) # Jan-Dec
cfsToAcFtMonth = 1 / 43560 * seconds_in_month # convert from cfs to AcFt/Month

obs_reg <- read.csv("daily_historical_flow_2018.csv", stringsAsFactors=F)
obs_reg$DATE <- as.Date(obs_reg$Date)
obs_nat <- read.csv("NRNI_2020_daily.csv", stringsAsFactors=F) 
obs_nat$DATE <- as.Date(obs_nat$Date, "%d-%B-%y")
nat_replacement <- read.csv("nat_flow_replacement.csv")
nat_replacement$DATE <- as.Date(paste(nat_replacement$Year, nat_replacement$Month, "01", sep="-"))
sim_reg <- read.table(paste0(run_type, "/Historical_baseline/dams_out.txt"), header=T, stringsAsFactors=F)
sim_reg[,-c(1:5)] <- sim_reg[,-c(1:5)] / 13.8843
sim_nat <- read.table(paste0("~/RColSim_v1/Preliminary/output/", run_type, "/Historical_baseline/supply_baseline.txt"), header=T) 
sim_nat[,-c(1:4)] <- sim_nat[,-c(1:4)] / 13.8843
sim_nat$DATE <- as.Date(paste(sim_nat$Years, sim_nat$Months, sim_nat$Days, sep="-"))
stns <- names(sim_nat)[-c(1:4)][which(names(sim_nat)[-c(1:4)] %in% names(obs_reg) & names(sim_nat)[-c(1:4)] %in% names(sim_reg))]


begin_date <- as.Date("1979-01-01", "%Y-%m-%d")
end_date <- as.Date("2007-12-31", "%Y-%m-%d")
dates <- seq(from=begin_date, to=end_date, by=1)
weekly_obs_reg <- apply.weekly(xts(obs_reg[which(obs_reg$Date >= begin_date & obs_reg$Date <= end_date),-which(names(obs_reg) %in% c("Date", "DATE"))], dates), mean)
weekly_obs_nat <- apply.weekly(xts(obs_nat[which(obs_nat$DATE >= begin_date & obs_nat$DATE <= end_date),-which(names(obs_nat) %in% c("Date", "DATE"))], dates), mean)
weekly_dates <- row.names(as.data.frame(weekly_obs_reg))

start_wk <- as.Date("1979-07-31")
end_wk <- as.Date("2007-07-29")

obs_reg_weekly <- weekly_obs_reg[weekly_dates >= start_wk & weekly_dates <= end_wk,stns]
obs_nat_weekly <- weekly_obs_nat[weekly_dates >= start_wk & weekly_dates <= end_wk,stns]
sim_reg_weekly <- subset(sim_reg, as.Date(sim_reg$date) >= start_wk & as.Date(sim_reg$date) <= end_wk)[,stns]
sim_nat_weekly <- subset(sim_nat, DATE >= start_wk & DATE <= end_wk)[,stns]
var_names <- c("sim_nat", "sim_reg", "obs_reg", "obs_nat")
vars <- list(sim_nat_weekly, sim_reg_weekly, obs_reg_weekly, obs_nat_weekly)
ts_wk <- subset(sim_nat, DATE >= start_wk & DATE <= end_wk)[,c("Months", "Years", "DATE")]
ts_mo <- unique(subset(sim_nat, DATE >= start_wk & DATE <= end_wk)[,c("Months", "Years")])
ts_mo <- cbind(ts_mo, DATE=as.Date(paste(ts_mo$Years, ts_mo$Months, "01", sep="-")))
for(i in 1:length(vars)) {
	weekly_data <- data.frame(ts_wk, vars[[i]])
	assign(paste(var_names[i], "weekly", sep="_"), weekly_data[,-c(1:2)])
	assign(paste(var_names[i], "monthly", sep="_"), data.frame(DATE=ts_mo$DATE, aggregate(weekly_data[-c(1:3)], list(weekly_data$Months, weekly_data$Years), mean)[,-c(1:2)]))
}
#nat_replacement <- subset(nat_replacement, DATE>=start_wk & DATE<=end_wk)
#obs_nat_monthly[,c("BROWN", "LGOOS", "LMONU", "LGRAN")] <- nat_replacement[,c("BROWN", "LGOOS", "LMONU", "LGRAN")]

obs_sig_monthly <- cbind(DATE=ts_mo$DATE, obs_reg_monthly[,-1] - obs_nat_monthly[,-1])
sim_sig_monthly <- cbind(DATE=ts_mo$DATE, sim_reg_monthly[,-1] - sim_nat_monthly[,-1])
obs_sig_mean_monthly <- aggregate(obs_sig_monthly[,-1], list(ts_mo$Months), mean)
sim_sig_mean_monthly <- aggregate(sim_sig_monthly[,-1], list(ts_mo$Months), mean)
obs_reg_mean_monthly <- aggregate(obs_reg_monthly[,-1], list(ts_mo$Months), mean)
sim_reg_mean_monthly <- aggregate(sim_reg_monthly[,-1], list(ts_mo$Months), mean)

sim_sig_mean_monthly[,-1] <- sim_sig_mean_monthly[,-1] * cfsToAcFtMonth / 1e6
obs_sig_mean_monthly[,-1] <- obs_sig_mean_monthly[,-1] * cfsToAcFtMonth / 1e6

xmin <- as.Date("1979-08-01")
xmax <- as.Date("2007-07-31")


month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


eval_plot <- function(flow, timestep) {
  assign("obs", get(paste0("obs_", flow, "_", timestep)))
  assign("sim", get(paste0("sim_", flow, "_", timestep)))
  for (s in stns) {
	if (flow == "sig") {
		ymin <- 1.4 * min(c(sim[,s]), obs[,s])
		ylab <- "Change in Storage (million acre-ft)"
	} else {
		ymin <- 0 
		ylab <- "Flow (cfs)"
	}
	ymax <- 1.4 * max(c(sim[,s]), obs[,s]) 
	par(mar=c(0,0,0,0))
	if (timestep == "mean_monthly") {
		svg(paste0(plot_dir, flow, "_", timestep, "_", s, ".svg"), width=7, height=5, pointsize=12)
			plot(obs[,s] ~ obs[,1], ylim=c(ymin, ymax), type='l', ylab=ylab, xaxt="n", xlab="")
			grid(col="snow2", lty="solid")
			axis(1, at=1:12, labels=month_names)
			lines(obs[,s] ~ obs[,1], lty=1, col="black")
			lines(sim[,s] ~ sim[,1], lty=2, col="red")
		dev.off()
	} else {
		svg(paste0(plot_dir, flow, "_", timestep, "_", s, ".svg"), width=7, height=5, pointsize=12)
			plot(obs[,s] ~ obs$DATE, ylim=c(ymin, ymax), xlim=c(xmin,xmax), type='l', ylab="cfs", xlab="Date")
			grid(col="lightgrey", lty="solid")
			lines(sim[,s] ~ sim[,1], lty=2, col="red")
		dev.off()
	}
  }
}

svg(paste0(plot_dir, "reservoir_signatures.svg"), width=10, height=12, pointsize=16)
par(mfrow=c(4,2), mar=c(3,5,5,3), las=2, cex.main=1.2, font.main=1)
sim <- sim_sig_mean_monthly
obs <- obs_sig_mean_monthly
res_names <- c("Mica", "Hugh Keenleyside", "Libby", "Duncan", "Brownlee", "Dworshak", "Grand Coulee", "Hungry Horse")
res_short_names <- c("MICAA", "ARROW", "LIBBY", "DUNCA", "BROWN", "DWORS", "GCOUL", "FLASF")
for (s in 1:length(res_short_names)) {
	stn <- res_short_names[s]
	ymax <- 1.4 * max(c(sim[,stn]), obs[,stn]) 
	ymin <- 1.4 * min(c(sim[,stn]), obs[,stn])
	title_n <- paste0("Obs. vs Sim. Reservoir Signature at ", res_names[s])
	plot(obs[,stn] ~ obs[,1], ylim=c(ymin, ymax), type='l', ylab="Change in Storage (million acre-ft)", main=title_n, xaxt="n", xlab="")
	grid(col="snow2", lty="solid")
	axis(1, at=1:12, labels=month_names)
	lines(obs[,stn] ~ obs[,1], lty=1, col="black")
	lines(sim[,stn] ~ sim[,1], lty=2, col="red")
}
dev.off()
			
	
png(paste0(plot_dir, "reservoir_signatures.png"), res=400, width=4000, height=5000, units="px", pointsize=16)
par(mfrow=c(4,2), mar=c(3,5,5,3), las=2, cex.main=1.2, font.main=1)
sim <- sim_sig_mean_monthly
obs <- obs_sig_mean_monthly
res_names <- c("Mica", "Hugh Keenleyside", "Libby", "Duncan", "Brownlee", "Dworshak", "Grand Coulee", "Hungry Horse")
res_short_names <- c("MICAA", "ARROW", "LIBBY", "DUNCA", "BROWN", "DWORS", "GCOUL", "FLASF")
for (s in 1:length(res_short_names)) {
	stn <- res_short_names[s]
	ymax <- 1.4 * max(c(sim[,stn]), obs[,stn]) 
	ymin <- 1.4 * min(c(sim[,stn]), obs[,stn])
	title_n <- paste0("Obs. vs Sim. Reservoir Signature at ", res_names[s])
	plot(obs[,stn] ~ obs[,1], ylim=c(ymin, ymax), type='l', ylab="Change in Storage (million acre-ft)", main=title_n, xaxt="n", xlab="")
	grid(col="snow2", lty="solid")
	axis(1, at=1:12, labels=month_names)
	lines(obs[,stn] ~ obs[,1], lty=1, col="black")
	lines(sim[,stn] ~ sim[,1], lty=2, col="red")
}
dev.off()

### Lower Columbia Dams 

res_names <- c("Bonneville", "The Dalles", "John Day", "McNary")
res_short_names <- c("BONNE", "DALLE", "JDAYY", "MCNAR")
svg(paste0(plot_dir, "LowerColumbia_mean_reg.svg"), width=14, height=10, pointsize=22)
par(mfrow=c(2,2), mar=c(3,5,3,1), las=2, cex.main=1.1, font.main=1)
sim <- sim_reg_mean_monthly
sim[,-1] <- sim[,-1] / 1000
obs <- obs_reg_mean_monthly
obs[,-1] <- obs[,-1] / 1000
for (s in 1:length(res_short_names)) {
	stn <- res_short_names[s]
	ymax <- 1.2 * max(c(sim[,stn]), obs[,stn]) 
	ymin <- 0.8 * min(c(sim[,stn]), obs[,stn])
	title_n <- paste0("Obs. vs Sim. Dam Outflow at ", res_names[s])
	plot(obs[,stn] ~ obs[,1], ylim=c(ymin, ymax), type='l', ylab="Flow (kcfs)", main=title_n, xaxt="n", xlab="")
	grid(col="snow2", lty="solid")
	axis(1, at=1:12, labels=month_names)
	lines(obs[,stn] ~ obs[,1], lty=1, col="black", lwd=2)
	points(obs[,stn] ~ obs[,1], col="black", pch=16, cex=0.8)
	lines(sim[,stn] ~ sim[,1], lty=2, col="red", lwd=2)
	points(sim[,stn] ~ sim[,1], pch=16, cex=0.8, col="red")
}
dev.off()

### Lower Columbia Dams 

res_names <- c("Bonneville", "The Dalles", "John Day", "McNary")
res_short_names <- c("BONNE", "DALLE", "JDAYY", "MCNAR")
svg(paste0(plot_dir, "LowerColumbia_reg.svg"), width=14, height=10, pointsize=22)
par(mfrow=c(2,2), mar=c(3,5,3,1), las=2, cex.main=1.1, font.main=1)
sim <- sim_reg_monthly
sim[,-1] <- sim[,-1] / 1000
obs <- obs_reg_monthly
obs[,-1] <- obs[,-1] / 1000
for (s in 1:length(res_short_names)) {
	stn <- res_short_names[s]
	ymax <- 1.1 * max(c(sim[,stn]), obs[,stn]) 
	ymin <- 0.8 * min(c(sim[,stn]), obs[,stn])
	title_n <- paste0("Obs. vs Sim. Monthly Outflow at ", res_names[s])
	plot(obs[,stn] ~ obs$DATE, ylim=c(ymin, ymax), type='l', ylab="Flow (kcfs)", main=title_n, xlab="")
	grid(col="snow2", lty="solid")
	lines(obs[,stn] ~ obs[,1], lty=1, col="black", lwd=1.5)
	lines(sim[,stn] ~ sim[,1], lty=2, col="red", lwd=1.5)
}
dev.off()

### Middle Columbia Dams

res_names <- c("Grand Coulee", "Priest Rapids", "Rock Island", "Rocky Reach", "Wanapum", "Wells", "Chief Joseph")
res_short_names <- c("GCOUL", "PRIRA", "RISLA", "ROCKY", "WANAP", "WELLS", "CHIEF")
svg(paste0(plot_dir, "MiddleColumbia_mean_reg.svg"), width=12, height=14, pointsize=24)
par(mfrow=c(4,2), mar=c(3,5,3,1), las=2, cex.main=1.1, font.main=1)
sim <- sim_reg_mean_monthly
sim[,-1] <- sim[,-1] / 1000
obs <- obs_reg_mean_monthly
obs[,-1] <- obs[,-1] / 1000
for (s in 1:length(res_short_names)) {
	stn <- res_short_names[s]
	ymax <- 1.2 * max(c(sim[,stn]), obs[,stn]) 
	ymin <- 0.8 * min(c(sim[,stn]), obs[,stn])
	title_n <- paste0("Obs. vs Sim. Dam Outflow at ", res_names[s])
	plot(obs[,stn] ~ obs[,1], ylim=c(ymin, ymax), type='l', ylab="Flow (kcfs)", main=title_n, xaxt="n", xlab="")
	grid(col="snow2", lty="solid")
	axis(1, at=1:12, labels=month_names)
	lines(obs[,stn] ~ obs[,1], lty=1, col="black", lwd=2)
	points(obs[,stn] ~ obs[,1], col="black", pch=16, cex=0.8)
	lines(sim[,stn] ~ sim[,1], lty=2, col="red")
	points(sim[,stn] ~ sim[,1], col="red", pch=16, cex=0.8, lwd=2)
}
dev.off()

### Middle Columbia Dams

res_names <- c("Grand Coulee", "Priest Rapids", "Rock Island", "Rocky Reach", "Wanapum", "Wells", "Chief Joseph")
res_short_names <- c("GCOUL", "PRIRA", "RISLA", "ROCKY", "WANAP", "WELLS", "CHIEF")
svg(paste0(plot_dir, "MiddleColumbia_reg.svg"), width=12, height=14, pointsize=24)
par(mfrow=c(4,2), mar=c(3,5,3,1), las=2, cex.main=1.1, font.main=1)
sim <- sim_reg_monthly
sim[,-1] <- sim[,-1] / 1000
obs <- obs_reg_monthly
obs[,-1] <- obs[,-1] / 1000
for (s in 1:length(res_short_names)) {
	stn <- res_short_names[s]
	ymax <- 1.1 * max(c(sim[,stn]), obs[,stn]) 
	ymin <- 0.8 * min(c(sim[,stn]), obs[,stn])
	title_n <- paste0("Obs. vs Sim. Monthly Outflow at ", res_names[s])
	plot(obs[,stn] ~ obs$DATE, ylim=c(ymin, ymax), type='l', ylab="Flow (kcfs)", main=title_n, xlab="")
	grid(col="snow2", lty="solid")
	lines(obs[,stn] ~ obs[,1], lty=1, col="black", lwd=1.5)
	lines(sim[,stn] ~ sim[,1], lty=2, col="red", lwd=1.5)
}
dev.off()

### Pend Oreille

res_names <- c("Albeni Falls", "Boundary", "Cabinet Gorge", "Hungry Horse", "Kerr", "Noxon Rapids")
res_short_names <- c("ALBEN", "BOUND", "CABIN", "FLASF", "FLAPO", "NOXON")
svg(paste0(plot_dir, "PendOreille_mean_reg.svg"), width=13, height=11, pointsize=24)
par(mfrow=c(3,2), mar=c(3,5,3,1), las=2, cex.main=1.1, font.main=1)
sim <- sim_reg_mean_monthly
sim[,-1] <- sim[,-1] / 1000
obs <- obs_reg_mean_monthly
obs[,-1] <- obs[,-1] / 1000
for (s in 1:length(res_short_names)) {
	stn <- res_short_names[s]
	ymax <- 1.2 * max(c(sim[,stn]), obs[,stn]) 
	ymin <- 0.8 * min(c(sim[,stn]), obs[,stn])
	title_n <- paste0("Obs. vs Sim. Dam Outflow at ", res_names[s])
	plot(obs[,stn] ~ obs[,1], ylim=c(ymin, ymax), type='l', ylab="Flow (kcfs)", main=title_n, xaxt="n", xlab="")
	grid(col="snow2", lty="solid")
	axis(1, at=1:12, labels=month_names)
	lines(obs[,stn] ~ obs[,1], lty=1, col="black", lwd=2)
	points(obs[,stn] ~ obs[,1], pch=16, col="black", cex=0.8)
	lines(sim[,stn] ~ sim[,1], lty=2, col="red", lwd=2)
	points(sim[,stn] ~ sim[,1], pch=16, col="red", cex=0.8)
}
dev.off()

### Pend Oreille

res_names <- c("Albeni Falls", "Boundary", "Cabinet Gorge", "Hungry Horse", "Kerr", "Noxon Rapids")
res_short_names <- c("ALBEN", "BOUND", "CABIN", "FLASF", "FLAPO", "NOXON")
svg(paste0(plot_dir, "PendOreille_reg.svg"), width=13, height=11, pointsize=24)
par(mfrow=c(3,2), mar=c(3,5,3,1), las=2, cex.main=1.1, font.main=1)
sim <- sim_reg_monthly
sim[,-1] <- sim[,-1] / 1000
obs <- obs_reg_monthly
obs[,-1] <- obs[,-1] / 1000
for (s in 1:length(res_short_names)) {
	stn <- res_short_names[s]
	ymax <- 1.1 * max(c(sim[,stn]), obs[,stn]) 
	ymin <- 0.8 * min(c(sim[,stn]), obs[,stn])
	title_n <- paste0("Obs. vs Sim. Monthly Outflow at ", res_names[s])
	plot(obs[,stn] ~ obs$DATE, ylim=c(ymin, ymax), type='l', ylab="Flow (kcfs)", main=title_n, xlab="")
	grid(col="snow2", lty="solid")
	lines(obs[,stn] ~ obs$DATE, lty=1, lwd=1.5, col="black")
	lines(sim[,stn] ~ sim$DATE, lty=2, lwd=1.5, col="red")
}
dev.off()

### Snake River

res_names <- c("Brownlee", "Dworshak", "Hells Canyon", "Ice Harbor", "Little Goose", "Lower Monumental", "Lower Granite")
res_short_names <- c("BROWN", "DWORS", "HCANY", "ICEHA", "LGOOS", "LMONU", "LGRAN")
svg(paste0(plot_dir, "Snake_mean_reg.svg"), width=12, height=14, pointsize=22)
par(mfrow=c(4,2), mar=c(3,5,3,1), las=2, cex.main=1.1, font.main=1)
sim <- sim_reg_mean_monthly
sim[,-1] <- sim[,-1] / 1000
obs <- obs_reg_mean_monthly
obs[,-1] <- obs[,-1] / 1000
for (s in 1:length(res_short_names)) {
	stn <- res_short_names[s]
	ymax <- 1.2 * max(c(sim[,stn]), obs[,stn]) 
	ymin <- 0.8 * min(c(sim[,stn]), obs[,stn])
	title_n <- paste0("Obs. vs Sim. Dam Outflow at ", res_names[s])
	plot(obs[,stn] ~ obs[,1], ylim=c(ymin, ymax), type='l', ylab="Flow (kcfs)", main=title_n, xaxt="n", xlab="")
	grid(col="snow2", lty="solid")
	axis(1, at=1:12, labels=month_names)
	lines(obs[,stn] ~ obs[,1], lty=1, col="black", lwd=2)
	points(obs[,stn] ~ obs[,1], pch=16, col="black", cex=0.8)
	lines(sim[,stn] ~ sim[,1], lty=2, col="red", lwd=2)
	points(sim[,stn] ~ sim[,1], pch=16, col="red", cex=0.8)
}
dev.off()

### Snake River

res_names <- c("Brownlee", "Dworshak", "Hells Canyon", "Ice Harbor", "Little Goose", "Lower Monumental", "Lower Granite")
res_short_names <- c("BROWN", "DWORS", "HCANY", "ICEHA", "LGOOS", "LMONU", "LGRAN")
svg(paste0(plot_dir, "Snake_reg.svg"), width=12, height=14, pointsize=22)
par(mfrow=c(4,2), mar=c(3,5,3,1), las=2, cex.main=1, font.main=1)
sim <- sim_reg_monthly
sim[,-1] <- sim[,-1] / 1000
obs <- obs_reg_monthly
obs[,-1] <- obs[,-1] / 1000
for (s in 1:length(res_short_names)) {
	stn <- res_short_names[s]
	ymax <- 1.1 * max(c(sim[,stn]), obs[,stn]) 
	ymin <- 0.8 * min(c(sim[,stn]), obs[,stn])
	title_n <- paste0("Obs. vs Sim. Monthly Outflow at ", res_names[s])
	plot(obs[,stn] ~ obs$DATE, ylim=c(ymin, ymax), type='l', ylab="Flow (kcfs)", main=title_n, xlab="")
	grid(col="snow2", lty="solid")
	lines(obs[,stn] ~ obs$DATE, lty=1, col="black", lwd=1.5)
	lines(sim[,stn] ~ sim$DATE, lty=2, col="red", lwd=1.5)
}
dev.off()

### Upper Columbia

res_names <- c("Mica", "Revelstoke", "Hugh Keenleyside")
res_short_names <- c("MICAA", "REVEL", "ARROW")
svg(paste0(plot_dir, "UpperColumbia_mean_reg.svg"), width=7, height=10, pointsize=20)
par(mfrow=c(3,1), mar=c(3,5,3,1), las=2, cex.main=1.1, font.main=1)
sim <- sim_reg_mean_monthly
sim[,-1] <- sim[,-1] / 1000
obs <- obs_reg_mean_monthly
obs[,-1] <- obs[,-1] / 1000
for (s in 1:length(res_short_names)) {
	stn <- res_short_names[s]
	ymax <- 1.2 * max(c(sim[,stn]), obs[,stn]) 
	ymin <- 0.8 * min(c(sim[,stn]), obs[,stn])
	title_n <- paste0("Obs. vs Sim. Dam Outflow at ", res_names[s])
	plot(obs[,stn] ~ obs[,1], ylim=c(ymin, ymax), type='l', ylab="Flow (kcfs)", main=title_n, xaxt="n", xlab="")
	grid(col="snow2", lty="solid")
	axis(1, at=1:12, labels=month_names)
	lines(obs[,stn] ~ obs[,1], lty=1, col="black", lwd=2)
	points(obs[,stn] ~ obs[,1], pch=16, col="black", cex=0.8)
	lines(sim[,stn] ~ sim[,1], lty=2, col="red", lwd=2)
	points(sim[,stn] ~ sim[,1], pch=16, col="red", cex=0.8)
}
dev.off()

### Upper Columbia

res_names <- c("Mica", "Revelstoke", "Hugh Keenleyside")
res_short_names <- c("MICAA", "REVEL", "ARROW")
svg(paste0(plot_dir, "UpperColumbia_reg.svg"), width=7, height=10, pointsize=20)
par(mfrow=c(3,1), mar=c(3,5,3,1), las=2, cex.main=1.1, font.main=1)
sim <- sim_reg_monthly
sim[,-1] <- sim[,-1] / 1000
obs <- obs_reg_monthly
obs[,-1] <- obs[,-1] / 1000
for (s in 1:length(res_short_names)) {
	stn <- res_short_names[s]
	ymax <- 1.1 * max(c(sim[,stn]), obs[,stn]) 
	ymin <- 0.8 * min(c(sim[,stn]), obs[,stn])
	title_n <- paste0("Obs. vs Sim. Monthly Outflow at ", res_names[s])
	plot(obs[,stn] ~ obs$DATE, ylim=c(ymin, ymax), type='l', ylab="Flow (kcfs)", main=title_n, xlab="")
	grid(col="snow2", lty="solid")
	lines(obs[,stn] ~ obs[,1], lty=1, col="black", lwd=1.5)
	lines(sim[,stn] ~ sim[,1], lty=2, col="red", lwd=1.5)
}
dev.off()

# Kootenay River

res_names <- c("Libby", "Bonners Ferry", "Corra Linn", "Duncan")
res_short_names <- c("LIBBY", "BONFE", "CORRA", "DUNCA")
svg(paste0(plot_dir, "Kootenay_mean_reg.svg"), width=12, height=8, pointsize=20)
par(mfrow=c(2,2), mar=c(3,5,3,1), las=2, cex.main=1.1, font.main=1)
sim <- sim_reg_mean_monthly
sim[,-1] <- sim[,-1] / 1000
obs <- obs_reg_mean_monthly
obs[,-1] <- obs[,-1] / 1000
for (s in 1:length(res_short_names)) {
	stn <- res_short_names[s]
	ymax <- 1.2 * max(c(sim[,stn]), obs[,stn]) 
	ymin <- 0.8 * min(c(sim[,stn]), obs[,stn])
	title_n <- paste0("Obs. vs Sim. Dam Outflow at ", res_names[s])
	plot(obs[,stn] ~ obs[,1], ylim=c(ymin, ymax), type='l', ylab="Flow (kcfs)", main=title_n, xaxt="n", xlab="")
	grid(col="snow2", lty="solid")
	axis(1, at=1:12, labels=month_names)
	lines(obs[,stn] ~ obs[,1], lty=1, col="black", lwd=2)
	points(obs[,stn] ~ obs[,1], pch=16, col="black", cex=0.8)
	lines(sim[,stn] ~ sim[,1], lty=2, col="red", lwd=2)
	points(sim[,stn] ~ sim[,1], pch=16, col="red", cex=0.8)
}
dev.off()

# Kootenay River

res_names <- c("Libby", "Bonners Ferry", "Corra Linn", "Duncan")
res_short_names <- c("LIBBY", "BONFE", "CORRA", "DUNCA")
svg(paste0(plot_dir, "Kootenay_reg.svg"), width=12, height=8, pointsize=20)
par(mfrow=c(2,2), mar=c(3,5,3,1), las=2, cex.main=1, font.main=1)
sim <- sim_reg_monthly
sim[,-1] <- sim[,-1] / 1000
obs <- obs_reg_monthly
obs[,-1] <- obs[,-1] / 1000
for (s in 1:length(res_short_names)) {
	stn <- res_short_names[s]
	ymax <- 1.1 * max(c(sim[,stn]), obs[,stn]) 
	ymin <- 0.8 * min(c(sim[,stn]), obs[,stn])
	title_n <- paste0("Obs. vs Sim. Monthly Outflow at ", res_names[s])
	plot(obs[,stn] ~ obs$DATE, ylim=c(ymin, ymax), type='l', ylab="Flow (kcfs)", main=title_n, xlab="")
	grid(col="snow2", lty="solid")
	lines(obs[,stn] ~ obs[,1], lty=1, col="black", lwd=1.5)
	lines(sim[,stn] ~ sim[,1], lty=2, col="red", lwd=1.5)
}
dev.off()




















res_names <- c("Grand Coulee", "Priest Rapids", "Rock Island", "Rocky Reach", "Wanapum", "Wells", "Chief Joseph")
res_short_names <- c("GCOUL", "PRIRA", "RISLA", "ROCKY", "WANAP", "WELLS", "CHIEF")
png(paste0(plot_dir, "MiddleColumbia.png"), res=400, width=4000, height=5000, pointsize=16)
par(mfrow=c(4,2), mar=c(3,5,5,3), las=2, cex.main=1.2, font.main=1)
sim <- sim_reg_mean_monthly
sim[,-1] <- sim[,-1] / 1000
obs <- obs_reg_mean_monthly
obs[,-1] <- obs[,-1] / 1000
for (s in 1:length(res_short_names)) {
	stn <- res_short_names[s]
	ymax <- 1.4 * max(c(sim[,stn]), obs[,stn]) 
	ymin <- 1.4 * min(c(sim[,stn]), obs[,stn])
	title_n <- paste0("Obs. vs Sim. Dam Outflow at ", res_names[s])
	plot(obs[,stn] ~ obs[,1], ylim=c(ymin, ymax), type='l', ylab="Flow (kcfs)", main=title_n, xaxt="n", xlab="")
	grid(col="snow2", lty="solid")
	axis(1, at=1:12, labels=month_names)
	lines(obs[,stn] ~ obs[,1], lty=1, col="black")
	lines(sim[,stn] ~ sim[,1], lty=2, col="red")
}
dev.off()









eval_plot(flow="reg", timestep="monthly")
eval_plot(flow="reg", timestep="mean_monthly")
eval_plot(flow="sig", timestep="mean_monthly")


