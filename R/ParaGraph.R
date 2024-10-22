globalVariables(c("xMean", "SEM"))
#` Title: Determines if Data Meets Parametric Conditions and Analyzes and Graphs Accordingly
#'
#' This package takes a quantitative and a qualitative variable and determine if they meet the assumptions for a one-way anova. If so, an anova is run and results, along with a post-hoc tukey test are shown. If data does not meet parametric assumptions than a kruskall wallis and subsequent dunn test are displayed. Data can be graphed producing a bar graph depicting the mean and SEM of "x" with the bars coloured by "y". Compact letters are also displayed above each bar.
#' @param df A data frame.
#' @param x A numeric vector. The numerical variable name or column number.
#' @param y A character vector. The categorical variable name or column number.
#' @param remove_outliers A string. Optional, either "TRUE" or "FALSE". Defaults to "FALSE".
#' @param post_hoc A string. Determines if a post-hoc analysis is run. Optional, either "TRUE" or "FALSE". Defaults to "TRUE".
#' @param graph Determines if the output is a graph or the data frame. Optional, either "TRUE" or "FALSE". Defaults to "FALSE".
#' @returns A bar graph and/or analysis results.
#' @import ggplot2
#' @importFrom plotrix std.error
#' @import multcomp
#' @importFrom car leveneTest
#' @importFrom multcompView multcompLetters
#' @import dplyr
#' @importFrom FSA dunnTest
#' @importFrom rcompanion cldList
#' @importFrom stats TukeyHSD
#' @importFrom stats aov
#' @importFrom stats kruskal.test
#' @importFrom stats quantile
#' @importFrom stats shapiro.test
#' @examples
#' paragraph(iris, 1, 5, graph = "TRUE")
#'
#' paragraph(iris,"Sepal.Length", "Species", remove_outliers = "TRUE", graph = "FALSE")

paragraph <- function(df, x, y, remove_outliers = "FALSE", post_hoc = "TRUE", graph = "FALSE"){

  Dataset <- df

  if(is.numeric(x)){
    Dataset$x <- df[,x]
    yaxis_title <- colnames(df)[x]
  }

  if(is.numeric(y)){
    Dataset$y <- df[,y]
    xaxis_title <- colnames(df)[y]
  }

  if(is.character(y)){
    Dataset$y <- df %>% pull(y)
    xaxis_title <- colnames(df)[grep(y, colnames(df))]

  }

  if(is.character(x)){
    Dataset$x <- df %>% pull(x)
    yaxis_title <- colnames(df)[grep(x, colnames(df))]

  }

  if(remove_outliers == "TRUE"){

    Q1 <- quantile(Dataset$x, .25)

    Q3 <- quantile(Dataset$x, .75)

    IQR <- IQR(Dataset$x)

    Dataset <- subset(Dataset, x > (Q1 - 1.5*IQR) & x < (Q3 + 1.5*IQR))

  }

  #### Check ANOVA Assumptions
  Normality <- shapiro.test(Dataset$x) # Normaility (greter than P 0.05 is needed for ANOVA)
  Homogeneity <- leveneTest(x ~ y, data = Dataset) # Homogeneity (greter than P 0.05 is needed for ANOVA)

  if(Normality$p.value < 0.05){
    print("Data does not appear to be normally distributed")
  }
  if(Homogeneity[1,3] < 0.05){
    print("Data does not appear to have equal variances")
  }

  if(Normality$p.value < 0.05 | Homogeneity[1,3] < 0.05){
    Kruskal_Wallis <- kruskal.test(x ~ y, data = Dataset)
    print(Kruskal_Wallis)
    dunn_test <- dunnTest(Dataset$x ~ Dataset$y, method = "bonferroni")
    dunn_cld <- cldList(P.adj ~ Comparison, data=dunn_test$res)
    cld_letters <- dunn_cld$Letter

    if(Kruskal_Wallis$p.value > 0.05){
      print("Analysis shows no significant differences")
    }

    if(Kruskal_Wallis$p.value < 0.05){
      print("The analysis appears to be significant :)")
    }
    if(post_hoc == "TRUE"){
      print(dunn_test)
    }

  }


  if(Normality$p.value > 0.05 & Homogeneity[1,3] > 0.05){
    print("Anova assumptions of normality and homogeneity met")
    ANOVA <- aov(x ~y, data = Dataset)
    summ_aov <- summary(ANOVA)
    print(summ_aov)
    tukey_result <- TukeyHSD(ANOVA)
    groups <- multcompLetters(tukey_result$y[, "p adj"] < 0.05)
    cld_letters <- groups$Letters
    print(cld_letters)
    if(post_hoc == "TRUE"){
      print("Tukey analysis results")
      print(tukey_result)
    }
  }


  if(graph == "TRUE"){
    Dataset_Bar <- Dataset %>% group_by(y) %>% filter(!is.na(x)) %>% summarise(xMean = mean(x), SEM = std.error(x))

    ggplot(Dataset_Bar, aes(y,xMean, fill = y))+ geom_errorbar(aes(ymin = xMean, ymax = xMean + SEM), width = 0.5)+
      geom_col(colour = 'black')+
      theme_classic()+
      scale_x_discrete(name = xaxis_title)+
      scale_y_continuous(expand = c(0,0), limits = c(0,max(Dataset_Bar$xMean+Dataset_Bar$SEM)*1.3), name = yaxis_title)+
      theme(legend.position = "none")+
      geom_text(aes(label = cld_letters, y = (xMean+SEM)*1.1))
  }
}
