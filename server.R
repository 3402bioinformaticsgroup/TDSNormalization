# server.R
#install.packages("kernlab")
library(shiny)
library(pwr)

shinyServer(function(input, output){

output$input.view<-renderTable({
        file<-input$NanoStringData
        if(is.null(file))
        return(NULL)
        data<-as.matrix(read.csv(file$datapath, header=T, row.names=1))
        data[1:5,]
    })
# plot raw data %CV
output$plotRawCV<-renderPlot({
    file<-input$NanoStringData   # read in nanostring profile
    if(is.null(file))
    return(NULL)
    data<-as.matrix(read.csv(file$datapath, header=T, row.names=1))
    
    ref.file<-input$PanelReference   # read in panel reference
    if(is.null(ref.file))
    return(NULL)
    ref<-as.matrix(read.csv(ref.file$datapath, header=T))
    CodeClass<-ref[,1]                               #not necessary here
    positive<-data[grep("Positive", CodeClass),]   #not necessary here
    # assessing cv% before normalization
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV.raw<-matrix(rep(0, dim(data)[1]*2 ), ncol=2)
    rownames(percentCV.raw)<-rownames(data)
    colnames(percentCV.raw)<-c("condition1(%)","condition2(%)")
    for (i in 1:dim(data)[1]){
        avg1<-mean(data[i, bio.condition1])
        avg2<-mean(data[i, bio.condition2])
        sd1<-sd(data[i, bio.condition1])
        sd2<-sd(data[i, bio.condition2])
        percentCV.raw[i,1]<-(sd1/avg1)*100
        percentCV.raw[i,2]<-(sd2/avg2)*100
    }
    # assessing tag wise variation
    processedReadSummary.raw<-matrix(rep(0, dim(data)[1]*6 ), ncol=6)
    rownames(processedReadSummary.raw)<-rownames(data)
    colnames(processedReadSummary.raw)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(data)[1]){
        processedReadSummary.raw[i,1]<-mean(data[i, ])
        processedReadSummary.raw[i,2]<-sd(data[i, ])
        processedReadSummary.raw[i,3]<-mean(data[i, bio.condition1])
        processedReadSummary.raw[i,4]<-sd(data[i, bio.condition1])
        processedReadSummary.raw[i,5]<-mean(data[i, bio.condition2])
        processedReadSummary.raw[i,6]<-sd(data[i, bio.condition2])
    }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    plot(processedReadSummary.raw[,2]/processedReadSummary.raw[,1]*100, col=col, pch=pch, xaxt='n', ylab="CV%", main="Raw Data Tag CV% (all samples)")
    legend("topright", legend=c("Endogenous","Positive","Negative","HouseKeeping"), col=c("red","green","green","green"),pch=c(16,17,18,15))
})

# plot processed data %CV
output$plotProcessedCV<-renderPlot({
    file<-input$NanoStringData   # read in nanostring profile
    if(is.null(file))
    return(NULL)
    data<-as.matrix(read.csv(file$datapath, header=T, row.names=1))
    
    ref.file<-input$PanelReference   # read in panel reference
    if(is.null(ref.file))
    return(NULL)
    ref<-as.matrix(read.csv(ref.file$datapath, header=T))
    CodeClass<-ref[,1]                               #not necessary here
    positive<-data[grep("Positive", CodeClass),]   #not necessary here
    # assessing cv% before normalization
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV.raw<-matrix(rep(0, dim(data)[1]*2 ), ncol=2)
    rownames(percentCV.raw)<-rownames(data)
    colnames(percentCV.raw)<-c("condition1(%)","condition2(%)")
    for (i in 1:dim(data)[1]){
        avg1<-mean(data[i, bio.condition1])
        avg2<-mean(data[i, bio.condition2])
        sd1<-sd(data[i, bio.condition1])
        sd2<-sd(data[i, bio.condition2])
        percentCV.raw[i,1]<-(sd1/avg1)*100
        percentCV.raw[i,2]<-(sd2/avg2)*100
    }
    # assessing tag wise variation
    processedReadSummary.raw<-matrix(rep(0, dim(data)[1]*6 ), ncol=6)
    rownames(processedReadSummary.raw)<-rownames(data)
    colnames(processedReadSummary.raw)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(data)[1]){
        processedReadSummary.raw[i,1]<-mean(data[i, ])
        processedReadSummary.raw[i,2]<-sd(data[i, ])
        processedReadSummary.raw[i,3]<-mean(data[i, bio.condition1])
        processedReadSummary.raw[i,4]<-sd(data[i, bio.condition1])
        processedReadSummary.raw[i,5]<-mean(data[i, bio.condition2])
        processedReadSummary.raw[i,6]<-sd(data[i, bio.condition2])
    }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    
    #normalization-positive control
    positive.average<-colSums(positive)/dim(positive)[1]
    average<-mean(positive.average)
    ScalingFactor<-average/positive.average
    
    countsPosNormalize<-data
    for (i in 1:dim(data)[2]){
        countsPosNormalize[,i]<-data[,i]*ScalingFactor[i]
    }
    negative<-countsPosNormalize[grep("Negative", CodeClass),]
    
    # assessing cv% after positive control normalization
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV.pos<-matrix(rep(0, dim(countsPosNormalize)[1]*2 ), ncol=2)
    rownames(percentCV.pos)<-rownames(countsPosNormalize)
    colnames(percentCV.pos)<-c("condition1(%)","condition2(%)")
    for (i in 1:dim(countsPosNormalize)[1]){
        avg1<-mean(countsPosNormalize[i, bio.condition1])
        avg2<-mean(countsPosNormalize[i, bio.condition2])
        sd1<-sd(countsPosNormalize[i, bio.condition1])
        sd2<-sd(countsPosNormalize[i, bio.condition2])
        percentCV.pos[i,1]<-(sd1/avg1)*100
        percentCV.pos[i,2]<-(sd2/avg2)*100
        }
    # assessing tag wise variation
    processedReadSummary.pos<-matrix(rep(0, dim(countsPosNormalize)[1]*6 ), ncol=6)
    rownames(processedReadSummary.pos)<-rownames(countsPosNormalize)
    colnames(processedReadSummary.pos)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(countsPosNormalize)[1]){
        processedReadSummary.pos[i,1]<-mean(countsPosNormalize[i, ])
        processedReadSummary.pos[i,2]<-sd(countsPosNormalize[i, ])
        processedReadSummary.pos[i,3]<-mean(countsPosNormalize[i, bio.condition1])
        processedReadSummary.pos[i,4]<-sd(countsPosNormalize[i, bio.condition1])
        processedReadSummary.pos[i,5]<-mean(countsPosNormalize[i, bio.condition2])
        processedReadSummary.pos[i,6]<-sd(countsPosNormalize[i, bio.condition2])
    }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    #normalization-housekeeper
    gm_mean<-function(x){prod(x[x>0])^(1/length(x))}
    housekeeper<-countsPosNormalize[grep("Housekeeping", CodeClass),]
    housekeeper.gm_mean<-rep(0,dim(housekeeper)[2])
    for (i in 1:length(housekeeper.gm_mean)){
        housekeeper.gm_mean[i]<-gm_mean(housekeeper[,i])
    }
    average.housekeeper.gm_mean<-mean(housekeeper.gm_mean)
    NormalizationFactor<-average.housekeeper.gm_mean/housekeeper.gm_mean
    countsRefNormalize<-countsPosNormalize
    for (i in 1:dim(countsPosNormalize)[2]){
        for (j in c(grep("Endogenous", CodeClass),grep("Housekeeping", CodeClass))){
            countsRefNormalize[j,i]<-countsPosNormalize[j,i]*NormalizationFactor[i]
        }
    }
    
    # assessing cv% after housekeeping gene normalization
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV.ref<-matrix(rep(0, dim(countsRefNormalize)[1]*2 ), ncol=2)
    rownames(percentCV.ref)<-rownames(countsRefNormalize)
    colnames(percentCV.ref)<-c("condition1(%)","condition2(%)")
    for (i in 1:dim(countsRefNormalize)[1]){
        avg1<-mean(countsRefNormalize[i, bio.condition1])
        avg2<-mean(countsRefNormalize[i, bio.condition2])
        sd1<-sd(countsRefNormalize[i, bio.condition1])
        sd2<-sd(countsRefNormalize[i, bio.condition2])
        percentCV.ref[i,1]<-(sd1/avg1)*100
        percentCV.ref[i,2]<-(sd2/avg2)*100
    }
    # assessing tag wise variation
    processedReadSummary.ref<-matrix(rep(0, dim(countsRefNormalize)[1]*6 ), ncol=6)
    rownames(processedReadSummary.ref)<-rownames(countsRefNormalize)
    colnames(processedReadSummary.ref)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(countsRefNormalize)[1]){
        processedReadSummary.ref[i,1]<-mean(countsRefNormalize[i, ])
        processedReadSummary.ref[i,2]<-sd(countsRefNormalize[i, ])
        processedReadSummary.ref[i,3]<-mean(countsRefNormalize[i, bio.condition1])
        processedReadSummary.ref[i,4]<-sd(countsRefNormalize[i, bio.condition1])
        processedReadSummary.ref[i,5]<-mean(countsRefNormalize[i, bio.condition2])
        processedReadSummary.ref[i,6]<-sd(countsRefNormalize[i, bio.condition2])
        }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    # determine background threshhold
    a=2
    negative<-negative
    NegativeAverage<- colSums(negative)/dim(negative)[1]
    BackgroundLimit<-NegativeAverage
    for(i in 1:length(NegativeAverage)){
        BackgroundLimit[i]<-NegativeAverage[i]+a*(sd(negative[,i]))
    }
    
    countsNoBackground<-countsRefNormalize
    for(i in 1:dim(countsNoBackground)[2]){
        countsNoBackground[,i]<-countsRefNormalize[,i]-BackgroundLimit[i]
    }
    
    for (i in 1:dim (countsNoBackground)[1]){
        for (j in 1:dim(countsNoBackground)[2]){
            if(countsNoBackground[i,j]<=0){countsNoBackground[i,j]<-1}
        }
    }
    countsProcessed<-countsNoBackground     # here is the positive
    
    # percent of coefficient of variation
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV<-matrix(rep(0, dim(countsProcessed)[1]*2 ), ncol=2)
    rownames(percentCV)<-rownames(countsProcessed)
    colnames(percentCV)<-c("condition1(%)","condition2(%)")
    
    for (i in 1:dim(countsProcessed)[1]){
        avg1<-mean(countsProcessed[i, bio.condition1])
        avg2<-mean(countsProcessed[i, bio.condition2])
        sd1<-sd(countsProcessed[i, bio.condition1])
        sd2<-sd(countsProcessed[i, bio.condition2])
        percentCV[i,1]<-(sd1/avg1)*100
        percentCV[i,2]<-(sd2/avg2)*100
    }
    # assess tag-wise variation
    processedReadSummary<-matrix(rep(0, dim(countsProcessed)[1]*6 ), ncol=6)
    rownames(processedReadSummary)<-rownames(countsProcessed)
    colnames(processedReadSummary)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(countsProcessed)[1]){
        processedReadSummary[i,1]<-mean(countsProcessed[i, ])
        processedReadSummary[i,2]<-sd(countsProcessed[i, ])
        processedReadSummary[i,3]<-mean(countsProcessed[i, bio.condition1])
        processedReadSummary[i,4]<-sd(countsProcessed[i, bio.condition1])
        processedReadSummary[i,5]<-mean(countsProcessed[i, bio.condition2])
        processedReadSummary[i,6]<-sd(countsProcessed[i, bio.condition2])
    }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    plot(processedReadSummary[,2]/processedReadSummary[,1]*100, col=col, pch=pch, xaxt='n', ylab="CV%", main="Background removed Data Tag CV% (all samples)")
    legend("topright", legend=c("Endogenous","Positive","Negative","HouseKeeping"), col=c("red","green","green","green"),pch=c(16,17,18,15))
    abline(h=30, col="gray", lwd=3, lty=2)
})

# plot %CV along normalization process
output$plotCV<-renderPlot({
    file<-input$NanoStringData   # read in nanostring profile
    if(is.null(file))
    return(NULL)
    data<-as.matrix(read.csv(file$datapath, header=T, row.names=1))
    
    ref.file<-input$PanelReference   # read in panel reference
    if(is.null(ref.file))
    return(NULL)
    ref<-as.matrix(read.csv(ref.file$datapath, header=T))
    CodeClass<-ref[,1]                               #not necessary here
    positive<-data[grep("Positive", CodeClass),]   #not necessary here
    # assessing cv% before normalization
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV.raw<-matrix(rep(0, dim(data)[1]*2 ), ncol=2)
    rownames(percentCV.raw)<-rownames(data)
    colnames(percentCV.raw)<-c("condition1(%)","condition2(%)")
    for (i in 1:dim(data)[1]){
        avg1<-mean(data[i, bio.condition1])
        avg2<-mean(data[i, bio.condition2])
        sd1<-sd(data[i, bio.condition1])
        sd2<-sd(data[i, bio.condition2])
        percentCV.raw[i,1]<-(sd1/avg1)*100
        percentCV.raw[i,2]<-(sd2/avg2)*100
    }
    # assessing tag wise variation
    processedReadSummary.raw<-matrix(rep(0, dim(data)[1]*6 ), ncol=6)
    rownames(processedReadSummary.raw)<-rownames(data)
    colnames(processedReadSummary.raw)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(data)[1]){
        processedReadSummary.raw[i,1]<-mean(data[i, ])
        processedReadSummary.raw[i,2]<-sd(data[i, ])
        processedReadSummary.raw[i,3]<-mean(data[i, bio.condition1])
        processedReadSummary.raw[i,4]<-sd(data[i, bio.condition1])
        processedReadSummary.raw[i,5]<-mean(data[i, bio.condition2])
        processedReadSummary.raw[i,6]<-sd(data[i, bio.condition2])
    }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    
    #normalization-positive control
    positive.average<-colSums(positive)/dim(positive)[1]
    average<-mean(positive.average)
    ScalingFactor<-average/positive.average
    
    countsPosNormalize<-data
    for (i in 1:dim(data)[2]){
        countsPosNormalize[,i]<-data[,i]*ScalingFactor[i]
    }
    negative<-countsPosNormalize[grep("Negative", CodeClass),]
    
    # assessing cv% after positive control normalization
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV.pos<-matrix(rep(0, dim(countsPosNormalize)[1]*2 ), ncol=2)
    rownames(percentCV.pos)<-rownames(countsPosNormalize)
    colnames(percentCV.pos)<-c("condition1(%)","condition2(%)")
    for (i in 1:dim(countsPosNormalize)[1]){
        avg1<-mean(countsPosNormalize[i, bio.condition1])
        avg2<-mean(countsPosNormalize[i, bio.condition2])
        sd1<-sd(countsPosNormalize[i, bio.condition1])
        sd2<-sd(countsPosNormalize[i, bio.condition2])
        percentCV.pos[i,1]<-(sd1/avg1)*100
        percentCV.pos[i,2]<-(sd2/avg2)*100
    }
    # assessing tag wise variation
    processedReadSummary.pos<-matrix(rep(0, dim(countsPosNormalize)[1]*6 ), ncol=6)
    rownames(processedReadSummary.pos)<-rownames(countsPosNormalize)
    colnames(processedReadSummary.pos)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(countsPosNormalize)[1]){
        processedReadSummary.pos[i,1]<-mean(countsPosNormalize[i, ])
        processedReadSummary.pos[i,2]<-sd(countsPosNormalize[i, ])
        processedReadSummary.pos[i,3]<-mean(countsPosNormalize[i, bio.condition1])
        processedReadSummary.pos[i,4]<-sd(countsPosNormalize[i, bio.condition1])
        processedReadSummary.pos[i,5]<-mean(countsPosNormalize[i, bio.condition2])
        processedReadSummary.pos[i,6]<-sd(countsPosNormalize[i, bio.condition2])
    }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    #normalization-housekeeper
    gm_mean<-function(x){prod(x[x>0])^(1/length(x))}
    housekeeper<-countsPosNormalize[grep("Housekeeping", CodeClass),]
    housekeeper.gm_mean<-rep(0,dim(housekeeper)[2])
    for (i in 1:length(housekeeper.gm_mean)){
        housekeeper.gm_mean[i]<-gm_mean(housekeeper[,i])
    }
    average.housekeeper.gm_mean<-mean(housekeeper.gm_mean)
    NormalizationFactor<-average.housekeeper.gm_mean/housekeeper.gm_mean
    countsRefNormalize<-countsPosNormalize
    for (i in 1:dim(countsPosNormalize)[2]){
        for (j in c(grep("Endogenous", CodeClass),grep("Housekeeping", CodeClass))){
            countsRefNormalize[j,i]<-countsPosNormalize[j,i]*NormalizationFactor[i]
        }
    }
    
    # assessing cv% after housekeeping gene normalization
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV.ref<-matrix(rep(0, dim(countsRefNormalize)[1]*2 ), ncol=2)
    rownames(percentCV.ref)<-rownames(countsRefNormalize)
    colnames(percentCV.ref)<-c("condition1(%)","condition2(%)")
    for (i in 1:dim(countsRefNormalize)[1]){
        avg1<-mean(countsRefNormalize[i, bio.condition1])
        avg2<-mean(countsRefNormalize[i, bio.condition2])
        sd1<-sd(countsRefNormalize[i, bio.condition1])
        sd2<-sd(countsRefNormalize[i, bio.condition2])
        percentCV.ref[i,1]<-(sd1/avg1)*100
        percentCV.ref[i,2]<-(sd2/avg2)*100
    }
    # assessing tag wise variation
    processedReadSummary.ref<-matrix(rep(0, dim(countsRefNormalize)[1]*6 ), ncol=6)
    rownames(processedReadSummary.ref)<-rownames(countsRefNormalize)
    colnames(processedReadSummary.ref)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(countsRefNormalize)[1]){
        processedReadSummary.ref[i,1]<-mean(countsRefNormalize[i, ])
        processedReadSummary.ref[i,2]<-sd(countsRefNormalize[i, ])
        processedReadSummary.ref[i,3]<-mean(countsRefNormalize[i, bio.condition1])
        processedReadSummary.ref[i,4]<-sd(countsRefNormalize[i, bio.condition1])
        processedReadSummary.ref[i,5]<-mean(countsRefNormalize[i, bio.condition2])
        processedReadSummary.ref[i,6]<-sd(countsRefNormalize[i, bio.condition2])
    }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    # determine background threshhold
    a=2
    negative<-negative
    NegativeAverage<- colSums(negative)/dim(negative)[1]
    BackgroundLimit<-NegativeAverage
    for(i in 1:length(NegativeAverage)){
        BackgroundLimit[i]<-NegativeAverage[i]+a*(sd(negative[,i]))
    }
    
    countsNoBackground<-countsRefNormalize
    for(i in 1:dim(countsNoBackground)[2]){
        countsNoBackground[,i]<-countsRefNormalize[,i]-BackgroundLimit[i]
    }
    
    for (i in 1:dim (countsNoBackground)[1]){
        for (j in 1:dim(countsNoBackground)[2]){
            if(countsNoBackground[i,j]<=0){countsNoBackground[i,j]<-1}
        }
    }
    countsProcessed<-countsNoBackground     # here is the positive
    
    # percent of coefficient of variation
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV<-matrix(rep(0, dim(countsProcessed)[1]*2 ), ncol=2)
    rownames(percentCV)<-rownames(countsProcessed)
    colnames(percentCV)<-c("condition1(%)","condition2(%)")
    
    for (i in 1:dim(countsProcessed)[1]){
        avg1<-mean(countsProcessed[i, bio.condition1])
        avg2<-mean(countsProcessed[i, bio.condition2])
        sd1<-sd(countsProcessed[i, bio.condition1])
        sd2<-sd(countsProcessed[i, bio.condition2])
        percentCV[i,1]<-(sd1/avg1)*100
        percentCV[i,2]<-(sd2/avg2)*100
    }
    # assess tag-wise variation
    processedReadSummary<-matrix(rep(0, dim(countsProcessed)[1]*6 ), ncol=6)
    rownames(processedReadSummary)<-rownames(countsProcessed)
    colnames(processedReadSummary)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(countsProcessed)[1]){
        processedReadSummary[i,1]<-mean(countsProcessed[i, ])
        processedReadSummary[i,2]<-sd(countsProcessed[i, ])
        processedReadSummary[i,3]<-mean(countsProcessed[i, bio.condition1])
        processedReadSummary[i,4]<-sd(countsProcessed[i, bio.condition1])
        processedReadSummary[i,5]<-mean(countsProcessed[i, bio.condition2])
        processedReadSummary[i,6]<-sd(countsProcessed[i, bio.condition2])
    }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    plot(density(percentCV), lty=1, lwd=3, col="red", main="percentCV", xlab="Coefficient of Variance (%)")
    lines(density(percentCV.raw), lty=1, lwd=3, col="blue")
    lines(density(percentCV.pos), lty=2, lwd=2, col="green")
    lines(density(percentCV.ref), lty=2, lwd=2, col="dark red")
    legend("topright", legend=c("Normalizated Data","Raw Data","Normalize by Positive Control","Remove Background"), col=c("red","blue","green","dark red"), lty=c(1,1,2,2), lwd=c(3,3,2,2))
})

# Download data
output$downloadData <- downloadHandler(
filename = function() { paste(input$outputfile, '.csv', sep='') },
content = function(file) {
    
    files<-input$NanoStringData   # read in nanostring profile Don't confuse "file", named differentily
    if(is.null(files))
    return(NULL)
    data<-as.matrix(read.csv(files$datapath, header=T, row.names=1))
    
    ref.files<-input$PanelReference   # read in panel reference
    if(is.null(ref.files))
    return(NULL)
    ref<-as.matrix(read.csv(ref.files$datapath, header=T))
    CodeClass<-ref[,1]                               #not necessary here
    positive<-data[grep("Positive", CodeClass),]   #not necessary here
    # assessing cv% before normalization
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV.raw<-matrix(rep(0, dim(data)[1]*2 ), ncol=2)
    rownames(percentCV.raw)<-rownames(data)
    colnames(percentCV.raw)<-c("condition1(%)","condition2(%)")
    for (i in 1:dim(data)[1]){
        avg1<-mean(data[i, bio.condition1])
        avg2<-mean(data[i, bio.condition2])
        sd1<-sd(data[i, bio.condition1])
        sd2<-sd(data[i, bio.condition2])
        percentCV.raw[i,1]<-(sd1/avg1)*100
        percentCV.raw[i,2]<-(sd2/avg2)*100
    }
    # assessing tag wise variation
    processedReadSummary.raw<-matrix(rep(0, dim(data)[1]*6 ), ncol=6)
    rownames(processedReadSummary.raw)<-rownames(data)
    colnames(processedReadSummary.raw)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(data)[1]){
        processedReadSummary.raw[i,1]<-mean(data[i, ])
        processedReadSummary.raw[i,2]<-sd(data[i, ])
        processedReadSummary.raw[i,3]<-mean(data[i, bio.condition1])
        processedReadSummary.raw[i,4]<-sd(data[i, bio.condition1])
        processedReadSummary.raw[i,5]<-mean(data[i, bio.condition2])
        processedReadSummary.raw[i,6]<-sd(data[i, bio.condition2])
    }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    
    #normalization-positive control
    positive.average<-colSums(positive)/dim(positive)[1]
    average<-mean(positive.average)
    ScalingFactor<-average/positive.average
    
    countsPosNormalize<-data
    for (i in 1:dim(data)[2]){
        countsPosNormalize[,i]<-data[,i]*ScalingFactor[i]
    }
    negative<-countsPosNormalize[grep("Negative", CodeClass),]
    
    # assessing cv% after positive control normalization
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV.pos<-matrix(rep(0, dim(countsPosNormalize)[1]*2 ), ncol=2)
    rownames(percentCV.pos)<-rownames(countsPosNormalize)
    colnames(percentCV.pos)<-c("condition1(%)","condition2(%)")
    for (i in 1:dim(countsPosNormalize)[1]){
        avg1<-mean(countsPosNormalize[i, bio.condition1])
        avg2<-mean(countsPosNormalize[i, bio.condition2])
        sd1<-sd(countsPosNormalize[i, bio.condition1])
        sd2<-sd(countsPosNormalize[i, bio.condition2])
        percentCV.pos[i,1]<-(sd1/avg1)*100
        percentCV.pos[i,2]<-(sd2/avg2)*100
    }
    # assessing tag wise variation
    processedReadSummary.pos<-matrix(rep(0, dim(countsPosNormalize)[1]*6 ), ncol=6)
    rownames(processedReadSummary.pos)<-rownames(countsPosNormalize)
    colnames(processedReadSummary.pos)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(countsPosNormalize)[1]){
        processedReadSummary.pos[i,1]<-mean(countsPosNormalize[i, ])
        processedReadSummary.pos[i,2]<-sd(countsPosNormalize[i, ])
        processedReadSummary.pos[i,3]<-mean(countsPosNormalize[i, bio.condition1])
        processedReadSummary.pos[i,4]<-sd(countsPosNormalize[i, bio.condition1])
        processedReadSummary.pos[i,5]<-mean(countsPosNormalize[i, bio.condition2])
        processedReadSummary.pos[i,6]<-sd(countsPosNormalize[i, bio.condition2])
    }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    #normalization-housekeeper
    gm_mean<-function(x){prod(x[x>0])^(1/length(x))}
    housekeeper<-countsPosNormalize[grep("Housekeeping", CodeClass),]
    housekeeper.gm_mean<-rep(0,dim(housekeeper)[2])
    for (i in 1:length(housekeeper.gm_mean)){
        housekeeper.gm_mean[i]<-gm_mean(housekeeper[,i])
    }
    average.housekeeper.gm_mean<-mean(housekeeper.gm_mean)
    NormalizationFactor<-average.housekeeper.gm_mean/housekeeper.gm_mean
    countsRefNormalize<-countsPosNormalize
    for (i in 1:dim(countsPosNormalize)[2]){
        for (j in c(grep("Endogenous", CodeClass),grep("Housekeeping", CodeClass))){
            countsRefNormalize[j,i]<-countsPosNormalize[j,i]*NormalizationFactor[i]
        }
    }
    
    # assessing cv% after housekeeping gene normalization
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV.ref<-matrix(rep(0, dim(countsRefNormalize)[1]*2 ), ncol=2)
    rownames(percentCV.ref)<-rownames(countsRefNormalize)
    colnames(percentCV.ref)<-c("condition1(%)","condition2(%)")
    for (i in 1:dim(countsRefNormalize)[1]){
        avg1<-mean(countsRefNormalize[i, bio.condition1])
        avg2<-mean(countsRefNormalize[i, bio.condition2])
        sd1<-sd(countsRefNormalize[i, bio.condition1])
        sd2<-sd(countsRefNormalize[i, bio.condition2])
        percentCV.ref[i,1]<-(sd1/avg1)*100
        percentCV.ref[i,2]<-(sd2/avg2)*100
    }
    # assessing tag wise variation
    processedReadSummary.ref<-matrix(rep(0, dim(countsRefNormalize)[1]*6 ), ncol=6)
    rownames(processedReadSummary.ref)<-rownames(countsRefNormalize)
    colnames(processedReadSummary.ref)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(countsRefNormalize)[1]){
        processedReadSummary.ref[i,1]<-mean(countsRefNormalize[i, ])
        processedReadSummary.ref[i,2]<-sd(countsRefNormalize[i, ])
        processedReadSummary.ref[i,3]<-mean(countsRefNormalize[i, bio.condition1])
        processedReadSummary.ref[i,4]<-sd(countsRefNormalize[i, bio.condition1])
        processedReadSummary.ref[i,5]<-mean(countsRefNormalize[i, bio.condition2])
        processedReadSummary.ref[i,6]<-sd(countsRefNormalize[i, bio.condition2])
    }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    # determine background threshhold
    a=2
    negative<-negative
    NegativeAverage<- colSums(negative)/dim(negative)[1]
    BackgroundLimit<-NegativeAverage
    for(i in 1:length(NegativeAverage)){
        BackgroundLimit[i]<-NegativeAverage[i]+a*(sd(negative[,i]))
    }
    
    countsNoBackground<-countsRefNormalize
    for(i in 1:dim(countsNoBackground)[2]){
        countsNoBackground[,i]<-countsRefNormalize[,i]-BackgroundLimit[i]
    }
    
    for (i in 1:dim (countsNoBackground)[1]){
        for (j in 1:dim(countsNoBackground)[2]){
            if(countsNoBackground[i,j]<=0){countsNoBackground[i,j]<-1}
        }
    }
    countsProcessed<-countsNoBackground     # here is the positive
    
    # percent of coefficient of variation
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV<-matrix(rep(0, dim(countsProcessed)[1]*2 ), ncol=2)
    rownames(percentCV)<-rownames(countsProcessed)
    colnames(percentCV)<-c("condition1(%)","condition2(%)")
    
    for (i in 1:dim(countsProcessed)[1]){
        avg1<-mean(countsProcessed[i, bio.condition1])
        avg2<-mean(countsProcessed[i, bio.condition2])
        sd1<-sd(countsProcessed[i, bio.condition1])
        sd2<-sd(countsProcessed[i, bio.condition2])
        percentCV[i,1]<-(sd1/avg1)*100
        percentCV[i,2]<-(sd2/avg2)*100
    }
    # assess tag-wise variation
    processedReadSummary<-matrix(rep(0, dim(countsProcessed)[1]*6 ), ncol=6)
    rownames(processedReadSummary)<-rownames(countsProcessed)
    colnames(processedReadSummary)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(countsProcessed)[1]){
        processedReadSummary[i,1]<-mean(countsProcessed[i, ])
        processedReadSummary[i,2]<-sd(countsProcessed[i, ])
        processedReadSummary[i,3]<-mean(countsProcessed[i, bio.condition1])
        processedReadSummary[i,4]<-sd(countsProcessed[i, bio.condition1])
        processedReadSummary[i,5]<-mean(countsProcessed[i, bio.condition2])
        processedReadSummary[i,6]<-sd(countsProcessed[i, bio.condition2])
    }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    
    write.csv(countsProcessed, file)
    })

# Download reports
observeEvent(input$outputreport, {
output$pdfview <- renderUI({
    
    files<-input$NanoStringData   # read in nanostring profile Don't confuse "file", named differentily
    if(is.null(files))
    return(NULL)
    data<-as.matrix(read.csv(files$datapath, header=T, row.names=1))
    
    ref.files<-input$PanelReference   # read in panel reference
    if(is.null(ref.files))
    return(NULL)
    ref<-as.matrix(read.csv(ref.files$datapath, header=T))
    CodeClass<-ref[,1]                               #not necessary here
    positive<-data[grep("Positive", CodeClass),]   #not necessary here
    # assessing cv% before normalization
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV.raw<-matrix(rep(0, dim(data)[1]*2 ), ncol=2)
    rownames(percentCV.raw)<-rownames(data)
    colnames(percentCV.raw)<-c("condition1(%)","condition2(%)")
    for (i in 1:dim(data)[1]){
        avg1<-mean(data[i, bio.condition1])
        avg2<-mean(data[i, bio.condition2])
        sd1<-sd(data[i, bio.condition1])
        sd2<-sd(data[i, bio.condition2])
        percentCV.raw[i,1]<-(sd1/avg1)*100
        percentCV.raw[i,2]<-(sd2/avg2)*100
    }
    # assessing tag wise variation
    processedReadSummary.raw<-matrix(rep(0, dim(data)[1]*6 ), ncol=6)
    rownames(processedReadSummary.raw)<-rownames(data)
    colnames(processedReadSummary.raw)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(data)[1]){
        processedReadSummary.raw[i,1]<-mean(data[i, ])
        processedReadSummary.raw[i,2]<-sd(data[i, ])
        processedReadSummary.raw[i,3]<-mean(data[i, bio.condition1])
        processedReadSummary.raw[i,4]<-sd(data[i, bio.condition1])
        processedReadSummary.raw[i,5]<-mean(data[i, bio.condition2])
        processedReadSummary.raw[i,6]<-sd(data[i, bio.condition2])
    }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    
    #normalization-positive control
    positive.average<-colSums(positive)/dim(positive)[1]
    average<-mean(positive.average)
    ScalingFactor<-average/positive.average
    
    countsPosNormalize<-data
    for (i in 1:dim(data)[2]){
        countsPosNormalize[,i]<-data[,i]*ScalingFactor[i]
    }
    negative<-countsPosNormalize[grep("Negative", CodeClass),]
    
    # assessing cv% after positive control normalization
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV.pos<-matrix(rep(0, dim(countsPosNormalize)[1]*2 ), ncol=2)
    rownames(percentCV.pos)<-rownames(countsPosNormalize)
    colnames(percentCV.pos)<-c("condition1(%)","condition2(%)")
    for (i in 1:dim(countsPosNormalize)[1]){
        avg1<-mean(countsPosNormalize[i, bio.condition1])
        avg2<-mean(countsPosNormalize[i, bio.condition2])
        sd1<-sd(countsPosNormalize[i, bio.condition1])
        sd2<-sd(countsPosNormalize[i, bio.condition2])
        percentCV.pos[i,1]<-(sd1/avg1)*100
        percentCV.pos[i,2]<-(sd2/avg2)*100
    }
    # assessing tag wise variation
    processedReadSummary.pos<-matrix(rep(0, dim(countsPosNormalize)[1]*6 ), ncol=6)
    rownames(processedReadSummary.pos)<-rownames(countsPosNormalize)
    colnames(processedReadSummary.pos)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(countsPosNormalize)[1]){
        processedReadSummary.pos[i,1]<-mean(countsPosNormalize[i, ])
        processedReadSummary.pos[i,2]<-sd(countsPosNormalize[i, ])
        processedReadSummary.pos[i,3]<-mean(countsPosNormalize[i, bio.condition1])
        processedReadSummary.pos[i,4]<-sd(countsPosNormalize[i, bio.condition1])
        processedReadSummary.pos[i,5]<-mean(countsPosNormalize[i, bio.condition2])
        processedReadSummary.pos[i,6]<-sd(countsPosNormalize[i, bio.condition2])
    }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    #normalization-housekeeper
    gm_mean<-function(x){prod(x[x>0])^(1/length(x))}
    housekeeper<-countsPosNormalize[grep("Housekeeping", CodeClass),]
    housekeeper.gm_mean<-rep(0,dim(housekeeper)[2])
    for (i in 1:length(housekeeper.gm_mean)){
        housekeeper.gm_mean[i]<-gm_mean(housekeeper[,i])
    }
    average.housekeeper.gm_mean<-mean(housekeeper.gm_mean)
    NormalizationFactor<-average.housekeeper.gm_mean/housekeeper.gm_mean
    countsRefNormalize<-countsPosNormalize
    for (i in 1:dim(countsPosNormalize)[2]){
        for (j in c(grep("Endogenous", CodeClass),grep("Housekeeping", CodeClass))){
            countsRefNormalize[j,i]<-countsPosNormalize[j,i]*NormalizationFactor[i]
        }
    }
    
    # assessing cv% after housekeeping gene normalization
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV.ref<-matrix(rep(0, dim(countsRefNormalize)[1]*2 ), ncol=2)
    rownames(percentCV.ref)<-rownames(countsRefNormalize)
    colnames(percentCV.ref)<-c("condition1(%)","condition2(%)")
    for (i in 1:dim(countsRefNormalize)[1]){
        avg1<-mean(countsRefNormalize[i, bio.condition1])
        avg2<-mean(countsRefNormalize[i, bio.condition2])
        sd1<-sd(countsRefNormalize[i, bio.condition1])
        sd2<-sd(countsRefNormalize[i, bio.condition2])
        percentCV.ref[i,1]<-(sd1/avg1)*100
        percentCV.ref[i,2]<-(sd2/avg2)*100
    }
    # assessing tag wise variation
    processedReadSummary.ref<-matrix(rep(0, dim(countsRefNormalize)[1]*6 ), ncol=6)
    rownames(processedReadSummary.ref)<-rownames(countsRefNormalize)
    colnames(processedReadSummary.ref)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(countsRefNormalize)[1]){
        processedReadSummary.ref[i,1]<-mean(countsRefNormalize[i, ])
        processedReadSummary.ref[i,2]<-sd(countsRefNormalize[i, ])
        processedReadSummary.ref[i,3]<-mean(countsRefNormalize[i, bio.condition1])
        processedReadSummary.ref[i,4]<-sd(countsRefNormalize[i, bio.condition1])
        processedReadSummary.ref[i,5]<-mean(countsRefNormalize[i, bio.condition2])
        processedReadSummary.ref[i,6]<-sd(countsRefNormalize[i, bio.condition2])
    }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    # determine background threshhold
    a=2
    negative<-negative
    NegativeAverage<- colSums(negative)/dim(negative)[1]
    BackgroundLimit<-NegativeAverage
    for(i in 1:length(NegativeAverage)){
        BackgroundLimit[i]<-NegativeAverage[i]+a*(sd(negative[,i]))
    }
    
    countsNoBackground<-countsRefNormalize
    for(i in 1:dim(countsNoBackground)[2]){
        countsNoBackground[,i]<-countsRefNormalize[,i]-BackgroundLimit[i]
    }
    
    for (i in 1:dim (countsNoBackground)[1]){
        for (j in 1:dim(countsNoBackground)[2]){
            if(countsNoBackground[i,j]<=0){countsNoBackground[i,j]<-1}
        }
    }
    countsProcessed<-countsNoBackground     # here is the positive
    
    # percent of coefficient of variation
    bio.condition1<-as.numeric(unlist(strsplit(input$Condition1,","))) # define condition 1
    bio.condition2<-as.numeric(unlist(strsplit(input$Condition2,","))) # define condition 2
    percentCV<-matrix(rep(0, dim(countsProcessed)[1]*2 ), ncol=2)
    rownames(percentCV)<-rownames(countsProcessed)
    colnames(percentCV)<-c("condition1(%)","condition2(%)")
    
    for (i in 1:dim(countsProcessed)[1]){
        avg1<-mean(countsProcessed[i, bio.condition1])
        avg2<-mean(countsProcessed[i, bio.condition2])
        sd1<-sd(countsProcessed[i, bio.condition1])
        sd2<-sd(countsProcessed[i, bio.condition2])
        percentCV[i,1]<-(sd1/avg1)*100
        percentCV[i,2]<-(sd2/avg2)*100
    }
    # assess tag-wise variation
    processedReadSummary<-matrix(rep(0, dim(countsProcessed)[1]*6 ), ncol=6)
    rownames(processedReadSummary)<-rownames(countsProcessed)
    colnames(processedReadSummary)<-c("mean","SD","condition1.mean","condition1.SD","condition2.mean","condition2.SD")
    for (i in 1:dim(countsProcessed)[1]){
        processedReadSummary[i,1]<-mean(countsProcessed[i, ])
        processedReadSummary[i,2]<-sd(countsProcessed[i, ])
        processedReadSummary[i,3]<-mean(countsProcessed[i, bio.condition1])
        processedReadSummary[i,4]<-sd(countsProcessed[i, bio.condition1])
        processedReadSummary[i,5]<-mean(countsProcessed[i, bio.condition2])
        processedReadSummary[i,6]<-sd(countsProcessed[i, bio.condition2])
    }
    pch<-CodeClass
    col<-CodeClass
    for (i in 1:length(CodeClass)){
        if(grepl("Endogenous", CodeClass[i])){col[i]<-"red"}
        if(grepl("Positive", CodeClass[i])){col[i]<-"green"}
        if(grepl("Negative", CodeClass[i])){col[i]<-"green"}
        if(grepl("Housekeeping", CodeClass[i])){col[i]<-"green"}
        if(grepl("Endogenous", CodeClass[i])){pch[i]<-16}
        if(grepl("Positive", CodeClass[i])){pch[i]<-17}
        if(grepl("Negative", CodeClass[i])){pch[i]<-18}
        if(grepl("Housekeeping", CodeClass[i])){pch[i]<-15}
    }
    class(pch)<-"numeric"
    
    pdf("~/Downloads/report.pdf")
    plot(processedReadSummary.raw[,2]/processedReadSummary.raw[,1]*100, col=col, pch=pch, xaxt='n', ylab="CV%", main="Raw Data Tag CV% (all samples)")
    legend("topright", legend=c("Endogenous","Positive","Negative","HouseKeeping"), col=c("red","green","green","green"),pch=c(16,17,18,15))
    
    barplot(positive.average, col=c(rep("blue",20),rep("red",20),rep("gray",8)), main="Positive Control Average")
    abline(h=average, lty=2,lwd=2)
    legend("topright", legend=c("bio condition1","bio condition2", "Other"), col=c("blue","red","gray"), pch=c(15,15,15))
    barplot(ScalingFactor, col=c(rep("blue",20),rep("red",20),rep("gray",8)), main="Positive Control Scaling Factor")
    abline(h=c(0.3,3),col="gray", lty=2, lwd=2)
    legend("topright", legend=c("bio condition1","bio condition2", "Other"), col=c("blue","red","gray"), pch=c(15,15,15))
    
    plot(processedReadSummary.pos[,2]/processedReadSummary.pos[,1]*100, col=col, pch=pch, xaxt='n', ylab="CV%", main="Positive Scaled Data Tag CV% (all samples)")
    legend("topright", legend=c("Endogenous","Positive","Negative","HouseKeeping"), col=c("red","green","green","green"),pch=c(16,17,18,15))
    
    barplot(housekeeper.gm_mean, col=c(rep("blue",20),rep("red",20),rep("gray",8)), main="Housekeeper Geometric Mean")
    abline(h=average.housekeeper.gm_mean, lty=2,lwd=2)
    barplot(NormalizationFactor, col=c(rep("blue",20),rep("red",20),rep("gray",8)), main="Housekeeper Normalization Factor")
    abline(h=c(0.3,3),col="gray", lty=2, lwd=2)
    
    plot(processedReadSummary.ref[,2]/processedReadSummary.ref[,1]*100, col=col, pch=pch, xaxt='n', ylab="CV%", main="Housekeeper Normalized Data Tag CV% (all samples)")
    legend("topright", legend=c("Endogenous","Positive","Negative","HouseKeeping"), col=c("red","green","green","green"),pch=c(16,17,18,15))
    
    barplot(NegativeAverage, col=c(rep("blue",20),rep("red",20),rep("gray",8)), main="Negative Control Average")
    barplot(BackgroundLimit, col=c(rep("blue",20),rep("red",20),rep("gray",8)), main="Background Count Limit")
    
    plot(processedReadSummary[,2]/processedReadSummary[,1]*100, col=col, pch=pch, xaxt='n', ylab="CV%", main="Background removed Data Tag CV% (all samples)")
    legend("topright", legend=c("Endogenous","Positive","Negative","HouseKeeping"), col=c("red","green","green","green"),pch=c(16,17,18,15))
    abline(h=30, col="gray", lwd=3, lty=2)

plot(density(percentCV), lty=1, lwd=3, col="red", main="percentCV", xlab="Coefficient of Variance (%)")
lines(density(percentCV.raw), lty=1, lwd=3, col="blue")
lines(density(percentCV.pos), lty=2, lwd=2, col="green")
lines(density(percentCV.ref), lty=2, lwd=2, col="dark red")
legend("topright", legend=c("Normalizated Data","Raw Data","Normalize by Positive Control","Remove Background"), col=c("red","blue","green","dark red"), lty=c(1,1,2,2), lwd=c(3,3,2,2))

tags$iframe(style="height:600px; width:100%", src="myreport.pdf")
    dev.off()
     })
  })

})