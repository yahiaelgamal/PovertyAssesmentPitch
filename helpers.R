library(reshape)

compareTwoModels = function(model1, model2, test.data, dep.var,
                            model1.name='model1', model2.name='model2',
                            model1.type=NULL, model2.type=NULL) {

  if(!is.null(model1.type))
    model1.prediction = predict(model1, newdata=test.data, type=model1.type)
  else
    model1.prediction = predict(model1, newdata=test.data)

  if(!is.null(model2.type))
    model2.prediction = predict(model2, newdata=test.data, type=model2.type)
  else
    model2.prediction = predict(model2, newdata=test.data)


  #confusionMatrix(model1.prediction > 0.4, test.data[, dep.var])$byClass[['Balanced Accuracy']]
  #confusionMatrix(model2.prediction > 0.4, test.data[, dep.var])$byClass[['Balanced Accuracy']]

  model1.ROCRpred = prediction(model1.prediction, test.data[, dep.var]) # prediction object
  model1.tpr.fpr = performance(model1.ROCRpred, 'tpr', 'fpr')

  model2.ROCRpred = prediction(model2.prediction, test.data[, dep.var]) # prediction object
  model2.tpr.fpr = performance(model2.ROCRpred, 'tpr', 'fpr')


  # not the cleanest way, but will make generating graphs for exploring much much better
  df1 = data.frame(xvalues = model1.tpr.fpr@x.values[[1]], yvalues = model1.tpr.fpr@y.values[[1]])
  df2 = data.frame(xvalues = model2.tpr.fpr@x.values[[1]], yvalues = model2.tpr.fpr@y.values[[1]])
  ggplot(df1, aes(x = xvalues, y=yvalues)) +
    geom_line() +
    geom_line(data=df2, aes(xvalues, yvalues), color='red') +
    labs(title=paste("Comparison between" , model1.name, 'and', model2.name ), x='False Positive Rate', y='True Positive Rate')
}

drawPrecisionRecall = function(ROCRObject){
  precision = performance(ROCRObject, 'prec')@y.values[[1]]
  recall =  performance(ROCRObject, 'rec')@y.values[[1]]
  cutoffs  = performance(ROCRObject, 'prec')@x.values[[1]]

  df = data.frame(cutoff=cutoffs, precision=precision, recall=recall)
  df = melt(df, id='cutoff')

  # you can use the tpr/fpr style of graph using rocplot(performance(ROCRObject, 'prec', 'rec'))

  ggplot(df, aes(x=cutoff, color=variable, y=value)) + geom_line() + labs(title="Precision and Recall")
}

drawTprFpr = function(ROCRObject){
  tpr = performance(ROCRObject, 'tpr')@y.values[[1]]
  fpr =  performance(ROCRObject, 'fpr')@y.values[[1]]
  cutoffs  = performance(ROCRObject, 'tpr')@x.values[[1]]

  df = data.frame(cutoff=cutoffs, tpr=tpr, fpr=fpr)
  df = melt(df, id='cutoff')

  # you can use the tpr/fpr style of graph using rocplot(performance(ROCRObject, 'prec', 'rec'))

  ggplot(df, aes(x=cutoff, color=variable, y=value)) + geom_line() + labs(title="tpr/fpr")
}
