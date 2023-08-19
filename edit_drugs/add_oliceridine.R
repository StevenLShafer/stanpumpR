load("../data/sysdata.rda.org")

objects()

drugDefaults_global

names(drugDefaults_global)

sapply(drugDefaults_global, typeof)

d <- data.frame(Drug="oliceridine",
          Concentration.Units="ng", # /ml is added somewhere
          Bolus.Units="mg",
          Infusion.Units="mcg/kg/min",
          Default.Units="mg",
          Units="mg,mcg/kg/min",
          Color="#FF00CC",
          Lower=27.4-3.5*1.96,
          Upper=27.4+3.5*1.96,
          Typical=27.4,
          MEAC=27.4, # TODO
          endCe=27.4,
          endCeText="ventilation")

drugDefaults_global <- rbind(drugDefaults_global, d)

save(list=c("drugDefaults_global","eventDefaults"), file="s.rda")

q("no")
