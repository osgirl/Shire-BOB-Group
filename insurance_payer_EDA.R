##################
## read-in data  #
##################


# working directory on local host is : setwd("~/Desktop/shire")
# the file path is nex fixed, make sure to set the working directory


library(readr)
library(readxl)
library(data.table)
library(tidyverse)
################## read in short and NPI files
# short.txt:        details about Prescriptions and medications
# Customer_NPI.txt: NPI # and its corresponding Shire ID
Prescriber_info<-fread(file="Prescriber_Info.csv")
Prescription<-read.csv(file = "short.txt",sep = "|")
NPI<-read.csv(file = "Customer_NPI.txt",sep = "|")

################## read in new patients and swtich files
# New_Patient_Rxs.csv:                 New patients who use the Xiidra
# Restasis_to_xiidra_Switch_Rxs.csv:   Patients who switch from restasis to Xiidra


New_Patient<- fread("New_Patient_Rxs.csv")
Switch_Patient <- fread("Restasis_to_xiidra_Switch_Rxs.csv")
 # >65 poelple would have medicare.
# summerise the count for medicare
all.medicare<-New_Patient%>%filter(grepl("Medicare",BOB)&dispensed_date>="2016-07-01"&dispensed_date<="2018-03-31")%>%
  select(-POP_DESC)%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "Medicare")

# everything not medicare
all.commericial<-New_Patient%>%filter(!grepl("Medicare",BOB)&dispensed_date>="2016-07-01"&dispensed_date<="2018-03-31")
all.commericial$BOB<-gsub("\\-.*","",all.commericial$BOB)  # delete everything after hyphen





# summerise the count for blue cross
blue.cross<-all.commericial%>%filter(grepl("Blue",BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "blue cross")

# summerise the count for goverment/federate/department
Government<-all.commericial%>%filter(grepl(c("Government|Federal|Department"),BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "Government")

# summerise the count for state/city
State<-all.commericial%>%filter(grepl(c("State|City"),BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "State")

# tackle others
all.commericial<-all.commericial%>%filter(!grepl("Shire|Blue|Government|Federal|Department|State|City",BOB))%>%
  filter(!grepl("All Other Third Party",BOB))

CVS<-all.commericial%>%filter(grepl("CVS",BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "CVS Health")
  
epxScpt<-all.commericial%>%filter(grepl("Express Scripts",BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "Express Scripts")

cash<-all.commericial%>%filter(grepl("Cash",BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "Cash")

OptumRx<-all.commericial%>%filter(grepl("OptumRx",BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "OptumRx")

UnitedHealthcare<-all.commericial%>%filter(grepl("UnitedHealthcare",BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "UnitedHealthcare")

Aetna<-all.commericial%>%filter(grepl("Aetna",BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "Aetna")

Anthem<-all.commericial%>%filter(grepl("Anthem",BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "Anthem")

final.insureance<-rbind(all.medicare,CVS,epxScpt,blue.cross,cash,Government,OptumRx,UnitedHealthcare,Aetna,State,Anthem)

########start with the plot

# Get the levels for type in the required order
final.insureance$product_name = factor(final.insureance$product_name, levels = c("RESTASIS","XIIDRA"))

# Calculate the percentages
library(plyr)
library(ggplot2)
final.insureance = ddply(final.insureance, .(healthPlan), transform, percent = n/sum(n) * 100)

# Format the labels and calculate their positions
final.insureance = ddply(final.insureance, .(healthPlan), transform, pos = (cumsum(n) - 0.5 * n))
final.insureance$label = paste0(sprintf("%.0f", final.insureance$percent), "%")
final.insureance$n<-final.insureance$n/1000
colnames(final.insureance) <- c("Product", "n","healthPlan","percent","pos","label")

# plot

p<-ggplot(final.insureance, aes(x = reorder(healthPlan, -n), y = n, fill = Product)) +
  geom_bar(position = position_stack(), stat = "identity", width = .8) +
  scale_y_continuous("Number of Total Prescriptions(in thousands)")+scale_x_discrete("Parent Company of Insurance Plan")+ theme(axis.text.x = element_text(size=12, angle=30),axis.text.y = element_text(
                                                                     size=8))+
  theme(legend.title = element_text(size=10, 
                                    face="bold"))+
  theme(legend.text = element_text( size=10,face="bold"))+theme(legend.position = c(0.8, 0.8))
p

for.pp<-final.insureance[c(7,8,9,10,11,12,17,18,1,2,21,22,5,6,13,14,3,4,19,20, 15,16),]
pp<-ggplot(for.pp, aes(x =healthPlan, y = percent, fill = Product)) +
  geom_bar(stat = "identity", width = .8)+scale_fill_manual("Product", values = c("RESTASIS" = "mistyrose1", "XIIDRA" = "steelblue1"))+
  geom_text(aes(label = label), color="black",position = position_stack(vjust = 0.5), size = 5,family = "Arial")+
  scale_y_continuous(name = "Perentage of Total Prescriptions ",labels = c("0%","25%","50%","75%","100%"))+
  scale_x_discrete(name="Parent Company of Insurance Plan", limits=c("Cash","CVS Health","Express Scripts","Aetna","OptumRx","UnitedHealthcare","blue cross","Government","Anthem","State","Medicare"))+ 
  theme(axis.text.x = element_text(size=11, angle=30,family = "Arial",face = "bold"),axis.text.y = element_text( size=12,family = "Arial",face = "bold"))+ 
  theme(axis.title = element_text(size=16,face = "bold"))+
  theme(legend.title = element_text(size=10, face="bold"))+
  theme(legend.text = element_text( size=10,face="bold"))
pp





############################################## 
######      only for late quater   ###########
##############################################

# summerise the count for medicare
all.medicare<-New_Patient%>%filter(grepl("Medicare",BOB)&dispensed_date>="2018-02-14"&dispensed_date<="2018-03-31")%>%
  select(-POP_DESC)%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "Medicare")

# everything not medicare
all.commericial<-New_Patient%>%filter(!grepl("Medicare",BOB)&dispensed_date>="2018-02-14"&dispensed_date<="2018-03-31")
all.commericial$BOB<-gsub("\\-.*","",all.commericial$BOB)  # delete everything after hyphen





# summerise the count for blue cross
blue.cross<-all.commericial%>%filter(grepl("Blue",BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "blue cross")

# summerise the count for goverment/federate/department
Government<-all.commericial%>%filter(grepl(c("Government|Federal|Department"),BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "Government")

# summerise the count for state/city
State<-all.commericial%>%filter(grepl(c("State|City"),BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "State")

# tackle others
all.commericial<-all.commericial%>%filter(!grepl("Shire|Blue|Government|Federal|Department|State|City",BOB))%>%
  filter(!grepl("All Other Third Party",BOB))

CVS<-all.commericial%>%filter(grepl("CVS",BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "CVS Health")

epxScpt<-all.commericial%>%filter(grepl("Express Scripts",BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "Express Scripts")

cash<-all.commericial%>%filter(grepl("Cash",BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "Cash")

OptumRx<-all.commericial%>%filter(grepl("OptumRx",BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "OptumRx")

UnitedHealthcare<-all.commericial%>%filter(grepl("UnitedHealthcare",BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "UnitedHealthcare")

Aetna<-all.commericial%>%filter(grepl("Aetna",BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "Aetna")

Anthem<-all.commericial%>%filter(grepl("Anthem",BOB))%>%
  group_by(product_name)%>%
  summarise(n=n())%>%
  mutate(healthPlan = "Anthem")

final.insureance<-rbind(all.medicare,CVS,epxScpt,blue.cross,cash,Government,OptumRx,UnitedHealthcare,Aetna,State,Anthem)

########start with the plot

# Get the levels for type in the required order
final.insureance$product_name = factor(final.insureance$product_name, levels = c("RESTASIS","XIIDRA"))

# Calculate the percentages
library(plyr)
library(ggplot2)
final.insureance = ddply(final.insureance, .(healthPlan), transform, percent = n/sum(n) * 100)

# Format the labels and calculate their positions
final.insureance = ddply(final.insureance, .(healthPlan), transform, pos = (cumsum(n) - 0.5 * n))
final.insureance$label = paste0(sprintf("%.0f", final.insureance$percent), "%")
final.insureance$n<-final.insureance$n/1000
colnames(final.insureance) <- c("Product", "n","healthPlan","percent","pos","label")

# plot
library(ggthemes)
p<-ggplot(final.insureance, aes(x = reorder(healthPlan, -n), y = n, fill = Product)) +
  geom_bar(position = position_stack(), stat = "identity", width = .8) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4)+theme_fivethirtyeight((size=14))+
  scale_y_continuous("Prescription Number (in thousands)")+ theme(axis.text.x = element_text(face="bold",size=8, angle=30),axis.text.y = element_text(face="bold",
                                                                                                                                                      size=8))+
  theme(legend.title = element_text(size=10, 
                                    face="bold"))+
  theme(legend.text = element_text( size=10,face="bold")) 
p

pp<-ggplot(final.insureance, aes(x =healthPlan, y = percent, fill = Product)) +
  geom_bar(stat = "identity", width = .8)+scale_fill_manual("Product", values = c("RESTASIS" = "mistyrose1", "XIIDRA" = "steelblue1"))+
  geom_text(aes(label = label), color="black",position = position_stack(vjust = 0.5), size = 5,family = "Arial")+
  scale_y_continuous(name = "Perentage of Total Prescriptions ",labels = c("0%","25%","50%","75%","100%"))+
  scale_x_discrete(name="Parent Company of Insurance Plan", limits=c("Cash","CVS Health","Express Scripts","Aetna","OptumRx","UnitedHealthcare","blue cross","Government","Anthem","State","Medicare"))+ 
  theme(axis.text.x = element_text(size=11, angle=30,family = "Arial",face = "bold"),axis.text.y = element_text( size=12,family = "Arial",face = "bold"))+ 
  theme(axis.title = element_text(size=16,face = "bold"))+
  theme(legend.title = element_text(size=10, face="bold"))+
  theme(legend.text = element_text( size=10,face="bold"))
pp
