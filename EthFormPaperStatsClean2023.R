Allsamples=EthForm2023AllSamples
Graded=EthForm2023Graded
wilcox.test(Allsamples$TotalParaEth,Allsamples$TotalParaForm, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)
wilcox.test(Allsamples$PFGEth,Allsamples$PFGForm, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)
wilcox.test(Allsamples$DiversityEth,Allsamples$DiversityForm, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)
wilcox.test(Allsamples$FilaPFGEth,Allsamples$FilaPFGForm, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)
wilcox.test(Allsamples$StrongPFGEth,Allsamples$StrongPFGForm, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)
wilcox.test(Allsamples$FilaTotalEth,Allsamples$FilaTotalForm, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)
wilcox.test(Allsamples$StrongTotalEth,Allsamples$StrongTotalForm, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)
wilcox.test(Allsamples$AvRatingEth,Allsamples$AvRatingForm, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)


wilcox.test(Graded$Fila1Eth,Graded$Fila1Form, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)
wilcox.test(Graded$Fila2Eth,Graded$Fila2Form, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)
wilcox.test(Graded$Fila3Eth,Graded$Fila3Form, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)
wilcox.test(Graded$Strong1Eth,Graded$Strong1Form, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)
wilcox.test(Graded$Strong2Eth,Graded$Strong2Form, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)
wilcox.test(Graded$Strong3Eth,Graded$Strong3Form, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)
wilcox.test(Graded$Total3Eth,Graded$Total3Form, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)
wilcox.test(Graded$Total2Eth,Graded$Total2Form, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)
wilcox.test(Graded$Total1Eth,Graded$Total1Form, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)

chisq.test()
StrongData=Ethanol.formalin.Chi.Square.format.2023...Strongyle
FilaData=Ethanol.formalin.Chi.Square.format.2023...Filariopsis
table(StrongData$Preservative,StrongData$ParasiteStatus)
Preservative=c("Formalin","Formalin","Ethanol","Ethanol")
ParasiteStatus=c("Positive","Negative","Positive","Negative")
Frequency=c(18,3,18,3)

StrongDataChat=data.frame(Preservative=c("Formalin","Formalin","Ethanol","Ethanol"),ParasiteStatus=c("Positive","Negative","Positive","Negative"),Frequency=c(18,3,18,3))
contingency_table=matrix(StrongDataChat$Frequency,nrow=2,byrow=TRUE, dimnames=list(c("Formalin","Ethanol"),c("Positive","Negative")))
print(contingency_table)


FilaDataChat=data.frame(Preservative=c("Formalin","Formalin","Ethanol","Ethanol"),ParasiteStatus=c("Positive","Negative","Positive","Negative"),Frequency=c(21,0,21,0))
contingency_table2=matrix(FilaDataChat$Frequency,nrow=2,byrow=TRUE, dimnames=list(c("Formalin","Ethanol"),c("Positive","Negative")))
print(contingency_table2)
print(chisq.test(contingency_table2))

AverageRating=Ethanol.formalin.csv.2023.update...Graded
wilcox.test(AverageRating$FilaAvRatingEth,AverageRating$FilaAvRatingForm, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)
wilcox.test(AverageRating$StrongAvRatingEth,AverageRating$StrongAvRatingForm, paired=TRUE, mu=0, exact=F, conf.int = T, conf.level = 0.95)