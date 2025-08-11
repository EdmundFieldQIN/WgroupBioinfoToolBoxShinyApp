options(BioC_mirror="https://mirrors.tuna.tsinghua.edu.cn/bioconductor")
options(BioC_mirror="https://mirrors.westlake.edu.cn/bioconductor",timeout=1800)
# if (!require("BiocManager"))
#   install.packages("BiocManager",repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
# library(BiocManager)
# if (!require("DESeq2"))
#   BiocManager::install("DESeq2")
# if (!require("edgeR"))
#   BiocManager::install("edgeR")
# if (!require("clusterProfiler"))
#   BiocManager::install("clusterProfiler")
# if (!require(shiny))
#   install.packages("shiny",repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
# if (!require(ggplot2))
#   install.packages("ggplot2",repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
# if (!require(colourpicker))
#   install.packages("colourpicker",repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
# if (!require(ggrepel))
#   install.packages("ggrepel",repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
# if (!require(tidyr))
#   install.packages("tidyr",repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
# if (!require(aplot))
#   install.packages("aplot",repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
# if (!require(shinythemes))
#   install.packages("shinythemes",repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
# if (!require(shinyalert))
#   install.packages("shinyalert",repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
# if (!require(pheatmap))
#   install.packages("pheatmap",repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
# if (!require(corrplot))
#   install.packages("corrplot",repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
# if (!require(Hmisc))
#   install.packages("Hmisc",repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
library(DESeq2)
require(edgeR)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyalert)
library(shinyjqui)
library(shinybusy)
library(shinyBS)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggfun)
library(ggh4x)
library(cols4all)
require(colourpicker)
library(tidyr)
library(dplyr)
library(tibble)
library(readr)
library(magrittr)
library(htmltools)
library(aplot)
require(pheatmap)
require(corrplot)
require(Hmisc)
library(bslib)
library(openxlsx)
library(aPEAR)
library(clusterProfiler)
library(yulab.utils)
library(enrichplot)
library(org.At.tair.db)  # 拟南芥注释数据库
library(org.Osativa.eg.db)  # 水稻注释数据库


## 读取ID背景信息
os_data <- read_delim(system.file("extdata/Osativa_uniprotkb.txt", package = "org.Osativa.eg.db"), 
                      delim = "\t", escape_double = FALSE, 
                      na = "null", trim_ws = TRUE)
ath_data <- read_delim(system.file("extdata/Athaliana_uniprotkb.txt", package = "org.Osativa.eg.db"), 
                       delim = "\t", escape_double = FALSE, 
                       na = "null", trim_ws = TRUE) 



######home布局
homepage <- 
  tabPanel("Home",icon = icon("house",style="color: #74C0FC;"),
           
           # 高级 headerPanel 设计
           div(
             class = "advanced-header",
             style = "background: linear-gradient(135deg, #4a00e0 0%, #8e2de2 100%);
                      padding: 10px 20px;margin: -20px -20px 10px -20px;
                      position: relative;overflow: hidden;
                      box-shadow: 0 6px 20px rgba(0,0,0,0.15);",
             # 背景装饰元素
             div(
               style = "
                 position: absolute;top: -50%;
                 right: -20%; width: 400px;height: 400px;
                 background: rgba(255,255,255,0.1);
                 border-radius: 50%;animation: float 6s ease-in-out infinite;
               "
             ),
             div(
               style = "
                 position: absolute;
                 bottom: -30%;left: -10%;width: 300px;height: 300px;
                 background: rgba(255,255,255,0.05); border-radius: 50%;
                 animation: float 8s ease-in-out infinite reverse;
               "
             ),
             
             # 主标题内容
             div(
               style = "position: relative; z-index: 10;",
               div(
                 class = "header-content",
                 style = "text-align: center; color: white;",
                 
                 # 主标题
                 # 主标题 + 版本 横向排列
                 div(
                   style = "
                   display: flex;align-items: center;
                   justify-content: center;gap: 20px;
                   margin-bottom: 10px;",
                   h1(
                     "Wgroup Bioinfo ToolBox",
                     style = "
                     font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
                     font-weight: 800;font-size: 3.2rem;margin: 0;color: white;
                     text-shadow: 2px 2px 4px rgba(0,0,0,0.3);letter-spacing: 1px;
                     animation: slideInDown 0.8s ease-out;"
                   ),
                   div(
                     "v1.4.0",
                     style = "background: #ffffff20;color: #fff;
                     font-size: 1.6rem;font-weight: bold;padding: 6px 18px;border-radius: 50px;
                     border: 2px solid rgba(255,255,255,0.4);box-shadow: 0 4px 10px rgba(0,0,0,0.2);
                     backdrop-filter: blur(8px);letter-spacing: 1px;animation: slideInUp 0.8s ease-out 0.3s both;
                     transition: all 0.3s ease;"
                   )
                   
                 ),
                 
                 # 副标题
                 p(
                   "RNA-Seq下游分析工具箱 | 让生物信息学分析更简单",
                   style = "
                     font-size: 1.5rem;margin: 20px auto;
                     max-width: 600px;opacity: 0.9;
                     font-weight: 300;line-height: 1.6;
                     animation: fadeIn 1s ease-out 0.6s both;
                   "
                 )
               )
             ),
             
             # CSS 动画定义
             tags$style(HTML("
               @keyframes slideInDown {
                 from {
                   transform: translateY(-100px);
                   opacity: 0;
                 }
                 to {
                   transform: translateY(0);
                   opacity: 1;
                 }
               }
               
               @keyframes slideInUp {
                 from {
                   transform: translateY(50px);
                   opacity: 0;
                 }
                 to {
                   transform: translateY(0);
                   opacity: 1;
                 }
               }
               
               @keyframes fadeIn {
                 from {
                   opacity: 0;
                 }
                 to {
                   opacity: 1;
                 }
               }
               
               @keyframes float {
                 0%, 100% {
                   transform: translateY(0px);
                 }
                 50% {
                   transform: translateY(-20px);
                 }
               }
               
               .advanced-header {
                 transition: all 0.3s ease;
               }
               
               
               /* 响应式设计 */
               @media (max-width: 768px) {
                 .advanced-header h1 {
                   font-size: 2.5rem !important;
                 }
                 .advanced-header p {
                   font-size: 1.1rem !important;
                 }
               }
             "))
           ),
           
           # 原有的内容保持不变
           fluidRow(
             column(3,bsCollapse(
               id = "homepage_collapse1",
               open = "homepage_introduction0",
               bsCollapsePanel(
                 title = h3("INFOMATION",style = "font-weight:bold;font-family: 'times'; font-size:16pt"),
                 value = "homepage_introduction0",
                 style = "info",
                 # shinythemes::themeSelector(),
                 p("让你某些下游分析更加顺畅!:D",style = "font-family: 'times'; font-size:12pt;color:grey"),
                 br(),
                 strong("欢迎各位使用，请不要将本工具用于商业用途，谢谢！", style = "font-family: 'times'; font-size:16pt"),
                 br(),
                 strong("🙇🙇🙇", style = "font-family: 'times'; font-size:16pt"),
                 hr(),
                 p("声明：1、本工具设计框架来自农心工作室的RNAdiffAPP，重写添加了其他功能。2、水稻KEGG Term数据来源：砷在氟中。",style = "font-family: 'times'; font-size:14pt"),
                 br(),
                 p("TBtools插件版本基于R4.2.1开发，部分包功能不及新版本完善。如果有能力单独运行R脚本，可以下载本脚本新版并在本地运行：", a("Github Code.", href="https://github.com/EdmundFieldQIN/WgroupBioinfoToolBoxShinyApp",style = "font-family: 'times'; font-size:14pt",target = "_blank"),style = "font-family: 'times'; font-size:14pt"),
                 br(),
                 p("有任何问题或建议请反馈给：", a("Github Issues.", href="https://github.com/EdmundFieldQIN/WgroupBioinfoToolBoxShinyApp/issues",style = "font-family: 'times'; font-size:14pt",target = "_blank"), style = "font-family: 'times'; font-size:14pt"),
                 hr(),
                 hr(),
                 p(em("Email: 871729982@qq.com",style = "font-family: 'times'; font-size:12pt;color:grey")),
                 br(),
                 p(em("地址:华南农业大学15号楼岭南现代农业科学与技术广东省实验室",style = "font-family: 'times'; font-size:12pt;color:grey")),
                 br(),
                 p(em("水稻基因挖掘与利用研究团队",style = "font-family: 'times'; font-size:12pt;color:grey"))
               ))),
             column(9,bsCollapse(
               id = "homepage_collapse2",
               multiple = TRUE,
               open = c("homepage_introduction1","homepage_introduction2"),
               bsCollapsePanel(
                 title = h3("Wgroup Bioinfo ToolBox 简介",style = "font-weight:bold;font-family: 'times'; font-size:18pt"),
                 value = "homepage_introduction1",
                 style = "primary",
                 p("本工具箱主要用于更方便进行水稻与拟南芥",span("RNA-Seq 下游分析",style="font-weight:bold"), "，方便易用，省去各个网站游走的时间，同时附带水稻各类id信息。", style = "font-family: 'times'; font-size:14pt"),
                 p("本工具箱使用了 R 语言以及shiny R包搭建整体, 并使用clusterprofiler, ggplot2等R包完成分析与可视化，并使用TBtools实现本地化运行。", style = "font-family: 'times'; font-size:14pt")
               ),
               bsCollapsePanel(
                 title = h3("Wgroup Bioinfo ToolBox 可使用功能",style = "font-weight:bold;font-family: 'times'; font-size:18pt"),
                 value = "homepage_introduction2",
                 style = "primary",
                 p("分为以下几个部分", style = "font-family: 'times'; font-size:16pt"),
                 p(em("功能 1: 使用DESeq2或edgeR分析差异表达基因"), style = "font-family: 'times'; font-size:16pt"),
                 p(em("功能 2: Matrix矩阵标准规划及可视化，绘制相关性热图，或者只计算TPM/CPM/FPKM."), style = "font-family: 'times'; font-size:16pt"),
                 p(em("功能 3: 基于差异表达分析数据绘制火山图，符合格式的数据也可以"), style = "font-family: 'times'; font-size:16pt"),
                 p(em("功能 4: GO/KEGG富集分析，基于clusterProfiler包"), style = "font-family: 'times'; font-size:16pt"),
                 p(em("功能 5: 基于富集分析结果绘制可定义程度更高的可视化功能",span("(开发中)",style="font-weight:bold;color:red;font-family: 'SimSun'")), style = "font-family: 'times'; font-size:16pt"),
                 p(em("功能 6: 水稻/拟南芥各类ID信息提取"), style = "font-family: 'times'; font-size:16pt"),
                 p(em("功能 7: qRT-PCR计算器",span("(开发中)",style="font-weight:bold;color:red;font-family: 'SimSun'")), style = "font-family: 'times'; font-size:16pt"),
                 br(),
                 p("请查阅使用说明指南————",span("还没来得及写🫠🫠🫠",style="font-weight:bold;color:blue;font-family: 'SimSun'"), style = "font-family: 'times'; font-size:16pt")
               )
             ),hr(),
             div(div(p("Copyright Wgroup 2025. All rights reserved. Designed by EdmundFieldQIN.")),style = "font-family: 'times'; font-size:14pt"),
             hr()
             )
           )
  )

# homepage<-tabPanel("Home",icon = icon("house",style="color: #74C0FC;"),
#                    sidebarLayout(
#   sidebarPanel(
#     position = "left",
#     # style = "width: 20vw;",
#     width = 3,
#     shinythemes::themeSelector(),
#     h3("Wgroup Bioinfo ToolBox V1.3",style = "font-family: 'times'"),
#     p("让你某些下游分析更加顺畅!:D",style = "font-family: 'times'; font-size:12pt;color:grey"),
#     br(),
#     strong("欢迎各位使用，请不要将本工具用于商业用途，谢谢！", style = "font-family: 'times'; font-size:16pt"),
#     br(),
#     strong("🙇🙇🙇", style = "font-family: 'times'; font-size:16pt"),
#     hr(),
#     p("声明：1、本工具设计框架来自农心工作室的RNAdiffAPP，重写添加了其他功能。2、水稻KEGG Term数据来源：砷在氟中。",style = "font-family: 'times'; font-size:14pt"),
#     br(),
#     p("TBtools插件版本基于R4.2.1开发，部分包功能不及新版本完善。如果有能力单独运行R脚本，可以下载本脚本新版并在本地运行：", a("Github Code.", href="https://github.com/EdmundFieldQIN/WgroupBioinfoToolBoxShinyApp",style = "font-family: 'times'; font-size:14pt",target = "_blank"),style = "font-family: 'times'; font-size:14pt"),
#     br(),
#     p("有任何问题或建议请反馈给：", a("Github Issues.", href="https://github.com/EdmundFieldQIN/WgroupBioinfoToolBoxShinyApp/issues",style = "font-family: 'times'; font-size:14pt",target = "_blank"), style = "font-family: 'times'; font-size:14pt"),
#     hr(),
#     hr(),
#     p(em("Email: 871729982@qq.com",style = "font-family: 'times'; font-size:12pt;color:grey")),
#     br(),
#     p(em("地址:华南农业大学15号楼岭南现代农业科学与技术广东省实验室",style = "font-family: 'times'; font-size:12pt;color:grey")),
#     br(),
#     p(em("水稻基因挖掘与利用研究团队",style = "font-family: 'times'; font-size:12pt;color:grey"))
#   ),
#   mainPanel(
#     h3("Wgroup Bioinfo ToolBox是什么 ?",style = "font-weight:bold;font-family: 'times'; font-size:18pt"),
#     p("本工具箱主要用于更方便进行水稻与拟南芥",span("RNA-Seq 下游分析",style="font-weight:bold; font-size:16pt"), "，方便易用，省去各个网站游走的时间，同时附带水稻各类id信息。", style = "font-family: 'times'; font-size:14pt"),
#     
#     p("本工具箱使用了 R 语言以及shiny R包搭建整体, 使用clusterprofiler, ggplot2等R包完成分析与可视化，并可以利用TBtools实现本地化运行。", style = "font-family: 'times'; font-size:16pt"),
#     br(),
#     h3("Wgroup Bioinfo ToolBox可以做什么分析?",style = "font-weight:bold;font-family: 'times'; font-size:18pt"),
#     p("分为以下几个部分", style = "font-family: 'times'; font-size:16pt"),
#     p(em("功能 1: 使用DESeq2或edgeR分析差异表达基因"), style = "font-family: 'times'; font-size:16pt"),
#     p(em("功能 2: Matrix矩阵标准规划及可视化，绘制相关性热图，或者只计算TPM/CPM/FPKM."), style = "font-family: 'times'; font-size:16pt"),
#     p(em("功能 3: 基于差异表达分析数据绘制火山图，符合格式的数据也可以"), style = "font-family: 'times'; font-size:16pt"),
#     p(em("功能 4: GO/KEGG富集分析，基于clusterProfiler包"), style = "font-family: 'times'; font-size:16pt"),
#     p(em("功能 5: 基于富集分析结果绘制可定义程度更高的可视化功能",span("(开发中)",style="font-weight:bold;color:red;font-family: 'SimSun'")), style = "font-family: 'times'; font-size:16pt"),
#     p(em("功能 6: 水稻/拟南芥各类ID信息提取"), style = "font-family: 'times'; font-size:16pt"),
#     br(),
#     h3("怎么使用Wgroup Bioinfo ToolBox ?",style = "font-weight:bold;font-family: 'times'; font-size:18pt"),
#     p("请查阅使用说明指南————",span("还没来得及写🫠🫠🫠",style="font-weight:bold;color:blue;font-family: 'SimSun'"), style = "font-family: 'times'; font-size:16pt"),
#     br(),hr(),
#     div(div(p("Copyright Wgroup 2025. All rights reserved. Designed by EdmundFieldQIN.")),style = "font-family: 'times'; font-size:14pt"),
#     hr()
#   )
# )
# )
######DESeq2页面布局
page1 <- tabPanel("差异表达分析",icon = icon("up-right-and-down-left-from-center",style="color: #74C0FC;"),
                  sidebarLayout(
                    sidebarPanel(
                      width = 3,
                      div(
                        fileInput("matFile", "选择Reads Matrix文件")
                        ),
                      div(checkboxInput('header', '第一行作为表头', TRUE)),
                      radioButtons('sep','数据分隔符号',c(Tab='\t',Comma=',',Semicolon=';'),selected = '\t',inline=T),
                      div(fileInput("conditionFile","选择分组设计文件",accept = c(".txt"))),
                      fluidRow(column(6,textInput("control","Control组",value = "Control")),
                               column(6,textInput("case","Treatment组",value = "Treatment"))),
                      selectInput("dataset","选择 ALL or Up or Down",choices = c("ALL","Up","Down")),
                      selectInput("tools","选择DESeq2还是edgeR ?",choices = c("DESeq2","edgeR")),
                      useShinyjs(),
                      fluidRow(column(6,actionButton("action","开始分析",icon = icon("play"),style = "color: white; background-color: #3498db; border-color: #2980b9;")),
                               column(6,actionButton("page1_clear_btn", "清空输入", icon = icon("trash"),style = "color: white; background-color: #e74c3c; border-color: #c0392b;"))
                      ),
                      hr(),
                      div(downloadButton("downlodData","下载结果",style = "color: white; background-color: #1abc9c; border-color: #16a085;"))
                      ),
                    mainPanel(
                      width = 9,
                      actionLink("win1","点击查看FeatureCounts定量数据格式(Count矩阵)"),
                      br(),
                      actionLink("win2","点击查看分组设计文件格式"),
                      div(helpText("结果展示"),
                          tags$hr(),
                          dataTableOutput("Result"))
                      )
                    )
                  )


######标准化与可视化布局
page2 <- tabPanel("Matrix矩阵标准化与可视化",icon = icon("align-center",style="color: #74C0FC;"),
                  sidebarLayout(
                    sidebarPanel(
                      width = 3,
                      fileInput("matrixFile", "选择Matrix矩阵文件"),
                      checkboxInput('headerT', '第一行作为表头', TRUE),
                      radioButtons('sepT','数据分隔符号',c(Tab='\t',Comma=',',Semicolon=';'),selected = '\t',inline=T),
                      radioButtons('plotOrcal','绘图 or 计算 ?',c(Plot='plot',Calculate='calculate'),selected = 'plot',inline=T),
                      br(),
                      selectInput("TP","热图 or 相关性分析图?",choices = c("heatmap","cor")),
                      fileInput("colgroupFile", "选择Column列分组信息"),
                      fileInput("rowgroupFile", "选择Row行分组信息"),
                      br(),
                      selectInput("cor_m","选择相关性计算方法",choices = c("pearson" ,"spearman")),
                      radioButtons('matNum','一个矩阵还是两个矩阵 ?',c(One='one',Two='two'),selected = 'one',inline=T),
                      fileInput("secondMatFile", "选择第二个Matrix矩阵文件"),
                      br(),
                      selectInput("CP","TPM, CPM or FPKM?",choices = c("TPM","CPM","FPKM")),
                      useShinyjs(),
                      fluidRow(column(4,actionButton("TPplotStart","开始绘图",icon = icon("play"),style = "color: white; background-color: #3498db; border-color: #2980b9;")),
                               column(4,actionButton("TPcalculateStart","开始计算",icon = icon("play"),style = "color: white; background-color: #3498db; border-color: #2980b9;")),
                               column(4,actionButton("page2_clear_btn", "清空输入", icon = icon("trash"),style = "color: white; background-color: #e74c3c; border-color: #c0392b;"))
                               )
                      ),
                    mainPanel(
                      width = 9,
                      splitLayout(
                        div(actionLink("winht","点击展示输入数据格式"),helpText("结果展示"),plotOutput("plot_ht"),helpText("输入数据预览"),tableOutput("table_tpm")),
                        div(colourpicker::colourInput("col_tp1","Higher Value 颜色","red"),
                            colourpicker::colourInput("col_tp2","Lower value 颜色","blue"),
                            colourpicker::colourInput("col_tp3","Middle Value 颜色","white"),
                            br(),
                            p("热图参数调整"),
                            fluidRow(column(6,checkboxInput('row_c', 'Column列聚类', TRUE)),
                                     column(6,checkboxInput('col_c', 'Row行聚类', TRUE))),
                            fluidRow(column(6,checkboxInput('col_a', '展示Column列注释', TRUE)),
                                     column(6,checkboxInput('row_a', '展示Row行注释', TRUE))),
                            br(),
                            div(p("相关性图参数调整"),
                                selectInput("corType"," Display full matrix, lower triangular or upper triangular matrix?",choices = c("full", "lower", "upper")),
                                selectInput("corMethod"," The visualization method of correlation matrix to be used.",choices = c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
                                selectInput("corOrder"," The ordering method of the correlation matrix.",choices = c("original", "AOE", "FPC", "hclust", "alphabet")),
                                colourpicker::colourInput("sig_col","Select p-value color","white"),
                                checkboxInput('corlim', 'col.lim will be c(-1, 1) when is.corr is TRUE, col.lim will be c(min(corr), max(corr)) when is.corr is FALSE', TRUE)),
                            br(),
                            p("下载参数调整"),
                            fluidRow(column(6,textInput("htwidth","Plot width",value = 7)),
                                     column(6,textInput("htheight","Plot height",value = 5))),
                            radioButtons('ext2ht', 'Plot output format',choices = c('PDF'='pdf',"PNG"='png', 'JPEG'='jpeg'), inline = T),
                            fluidRow(column(6,helpText("下载热图")),
                                     column(6,helpText("下载TPM/FPKM/CPM"))),
                            fluidRow(column(6,downloadButton("download_ht","Download",style = "color: white; background-color: #1abc9c; border-color: #16a085;")),
                                     column(6,downloadButton("download_tpm","Download",style = "color: white; background-color: #1abc9c; border-color: #16a085;")))
                            ),
                        cellWidths = c("70%","30%")
                        )
                      )
                    )
                  )


######火山图布局
page3 <- tabPanel("Volcanoplot", icon = icon("volcano",style="color: #74C0FC;"),
                  sidebarLayout(
                    sidebarPanel(width = 4,
                                 selectInput("volcanoplottype", "选择类型",
                                             choices = c("火山图" = "vol_norm",
                                                         "渐变火山图" = "vol_enhance",
                                                         "九象限火山图(两组)" = "vol_twogroup",
                                                         "多组火山图(未开放)" = "vol_multigroup")),
                                 conditionalPanel(
                                   condition = "input.volcanoplottype == 'vol_norm' || input.volcanoplottype == 'vol_enhance'",
                                   radioButtons("inputMethod", "选择数据输入类型:",
                                                choices = c("上传文件" = "file", "粘贴数据" = "text"),
                                                selected = "file", inline = TRUE),
                                   
                                   conditionalPanel(
                                     condition = "input.inputMethod == 'file'",
                                     fileInput("volcanoFile", "选择DESeq2 or edgeR 结果文件")
                                   ),
                                   conditionalPanel(
                                     condition = "input.inputMethod == 'text'",
                                     textAreaInput("volcanoText", "粘贴 DEG 结果 (TSV format)", rows = 8,
                                                   placeholder = "genes\tsampleA\tsampleB\tlog2FoldChange\tpvalue\tpadj")
                                   ),
                                   
                                   helpText("差异表达分析结果即可当作输入数据"),
                                   checkboxInput('header2', '第一行作为表头', TRUE),
                                   radioButtons('sep2', '数据分隔符号', c(Tab = '\t', Comma = ',', Semicolon = ';'), selected = '\t', inline = TRUE),
                                   
                                   fluidRow(
                                     column(4, textInput("name1", "选择geneID的列名", value = "geneid")),
                                     column(4, textInput("name2", "选择FDR的列名", value = "padj")),
                                     column(4, textInput("name3", "选择logFC的列名", value = "log2FoldChange"))
                                   )
                                 ),
                                 # vol_twogroup面板
                                 conditionalPanel(
                                   condition = "input.volcanoplottype == 'vol_twogroup'",
                                   
                                   ## 添加一个选择折叠框
                                   bsCollapse(
                                     id = "twogroup_collapse",
                                     open = "separate_input",  # 默认关闭
                                     bsCollapsePanel(
                                       title = "输入模式一：分别输入两组DESeq2结果文件",
                                       value = "separate_input",
                                       style = "info",
                                       helpText("差异表达分析结果即可当作输入数据(两组数据需要表头统一)"),
                                       fileInput("tgvolcanoFile1", "选择第一组DESeq2 or edgeR 结果文件"),
                                       fileInput("tgvolcanoFile2", "选择第二组DESeq2 or edgeR 结果文件"),
                                       fluidRow(
                                         column(4, textInput("tgname_geneid", "选择geneID的列名", value = "geneid")),
                                         column(4, textInput("tgname_fdr", "选择FDR的列名", value = "pvalue")),
                                         column(4, textInput("tgname_logfc", "选择logFC的列名", value = "log2FoldChange"))
                                       ),
                                       fluidRow(
                                         column(6, actionButton("datamergestart", "开始整合",icon = icon("play"),style = "color: white; background-color: orange; border-color: orange;")),
                                         column(6, downloadButton("downloadmergedData", "下载合并数据",style = "color: white; background-color: #1abc9c; border-color: #16a085;"))
                                       )
                                     ),
                                     bsCollapsePanel(
                                       title = "输入模式二：直接输入两组差异分析合并结果文件",
                                       value = "merge_input",
                                       style = "info",
                                       helpText("如果直接有这一份文件直接输入即可"),
                                       fileInput("volcanoFile_merged", "选择两组整合后的数据,包含内容：gene_id\\tFC1\\tFDR1\\tFC2\\tFDR2"),
                                       helpText("分别选择两组的绘图列名"),
                                       fluidRow(
                                         column(4, textInput("merged_name_geneid", "选择geneID的列名", value = "geneid")),
                                         column(4, textInput("merged_name_fdr1", "选择FDR_A的列名", value = "pvalue_1")),
                                         column(4, textInput("merged_name_logfc1", "选择logFC_A的列名", value = "log2FoldChange_1")),
                                         column(4, textInput("merged_name_fdr2", "选择FDR_B的列名", value = "pvalue_2")),
                                         column(4, textInput("merged_name_logfc2", "选择logFC_B的列名", value = "log2FoldChange_2"))
                                       )
                                     )
                                   ),
                                   checkboxInput('tgheader', '第一行作为表头', TRUE),
                                   radioButtons('tgsep', '数据分隔符号', 
                                                c(Tab = '\t', Comma = ',', Semicolon = ';'), 
                                                selected = '\t', inline = TRUE)
                                 ),
                                 
                                 # 通用参数
                                 textInput("fdrvalue", "设置FDR threshold阈值", value = 0.05),
                                 textInput("FCvalue", "设置logFC threshold阈值", value = 1),
                                 
                                 # 设置高亮 geneID list
                                 radioButtons("geneIDInputMethod", "选择高亮基因列表输入方式:",
                                              choices = c("上传文件" = "file", "粘贴列表" = "text"),
                                              selected = "file", inline = TRUE),
                                 
                                 conditionalPanel(
                                   condition = "input.geneIDInputMethod == 'file'",
                                   fileInput("geneIDlist", "选择 geneID list 文件（仅一列）")
                                 ),
                                 conditionalPanel(
                                   condition = "input.geneIDInputMethod == 'text'",
                                   textAreaInput("geneIDText", "粘贴 geneID 列表（每行一个）", rows = 5,
                                                 placeholder = "AT1G01010\nAT1G01020\n...\nor\nLOC_Os01g01010\nLOC_Os02g12340")
                                 ),
                                 useShinyjs(),
                                 fluidRow(column(6,actionButton("plotstart", "开始绘图",icon = icon("play"),style = "color: white; background-color: #3498db; border-color: #2980b9;")),
                                          column(6,actionButton("page3_clear_btn", "清空输入", icon = icon("trash"),style = "color: white; background-color: #e74c3c; border-color: #c0392b;"))
                                 )
                    ),
                    
                    mainPanel(
                      fluidPage(
                        width = 12,
                        style = "padding-left: 0; margin-left: 0px;padding-right: 0; margin-right: 0px;",
                        splitLayout(
                          div(
                            style = "overflow-y: auto;overflow-x: auto;",
                            actionLink("page3_showexampledata1","点击查看普通火山图输入数据格式"),
                            br(),
                            actionLink("page3_showexampledata2","点击查看九象限火山图输入文件格式"),
                            helpText("绘图展示"),
                            plotOutput("plot1", height = "60%"),
                            helpText("数据展示"),
                            dataTableOutput("text1")
                          ),
                          div(
                            # vol_norm的控件
                            conditionalPanel(
                              condition = "input.volcanoplottype == 'vol_norm'",
                              colourInput("col_down", "下调 genes 颜色", "#0f8b61"),
                              colourInput("col_nonsig", "不显著 genes 颜色", "gray50"),
                              colourInput("col_up", "上调 genes 颜色", "#ffa61d"),
                              sliderInput("alpha_norm", "Point 透明度", min = 0.1, max = 1, value = 1),
                              sliderInput("size_norm", "Points 大小", min = 0.5, max = 6, value = 2),
                              colourInput("textcol_norm", "文本颜色", "black"),
                              sliderInput("textsize_norm", "文本大小", min = 1, max = 7, value = 3),
                              textInput("title_norm", "设置title", value = "Volcano Plot")
                            ),
                            # vol_enhance的控件
                            conditionalPanel(
                              condition = "input.volcanoplottype == 'vol_enhance'",
                              helpText("渐变色板"),
                              fluidRow(
                                
                                column(6, colourInput("vecol1", "低", "#3288bd")),
                                column(6, colourInput("vecol2", "中低", "#66c2a5")),
                                column(6, colourInput("vecol3", "中", "#ffffbf")),
                                column(6, colourInput("vecol4", "中高", "#f46d43")),
                                column(6, colourInput("vecol5", "高", "#9e0142"))
                              ),
                              hr(),
                              fluidRow(
                                column(6, colourInput("veupcol", "上调label颜色", "#d73027")),
                                column(6, colourInput("vedowncol", "下调label颜色", "#3288bd"))
                              ),
                              sliderInput("veshowgenenum", "显示多少条基因(指定gene时本条失效)", min = 0, max = 10, value = 0),
                              fluidRow(
                                column(6, textInput("vey_increased", "y轴增加高度", value = 20)),
                                column(6, textInput("velabs_decreased", "label下降高度", value = 15))
                              ),
                              textInput("velabel_name", "设置label后缀", value = "Genes"),
                              textInput("vetitle", "设置title", value = "Volcano Plot")
                            ),
                            
                            
                            # vol_twogroup的控件
                            conditionalPanel(
                              condition = "input.volcanoplottype == 'vol_twogroup'",
                              fluidRow(
                                column(4, colourInput("tgcol1", "左上象限颜色", "#0f8b61")),
                                column(4, colourInput("tgcol2", "右上象限颜色", "red")),
                                column(4, colourInput("tgcol5", "中心象限颜色", "white"))
                              ),
                              fluidRow(
                                column(4, colourInput("tgcol3", "左下象限颜色", "blue")),
                                column(4, colourInput("tgcol4", "右下象限颜色", "#ffa61d")),
                                column(4, colourInput("tgcol6", "其余象限颜色", "black"))
                              ),
                              hr(),
                              fluidRow(
                                column(4, textInput("tgtext1", "左上象限标签", "A Down & B Up")),
                                column(4, textInput("tgtext2", "右上象限标签", "A Up & B Up")),
                                column(4, textInput("tgtext5", "中心象限标签", "Not Diff"))
                              ),
                              fluidRow(
                                column(4, textInput("tgtext3", "左下象限标签", "A Down & B Down")),
                                column(4, textInput("tgtext4", "右下象限标签", "A Up & B Down")),
                                column(4, textInput("tgtext6", "其余象限标签", "No Sig"))
                              ),
                              hr(),
                              sliderInput("alpha_twogroup", "Point 透明度", min = 0.1, max = 1, value = 0.5),
                              sliderInput("size_twogroup", "Points 大小", min = 0.5, max = 6, value = 2),
                              fluidRow(
                                column(4, checkboxInput('tgshowlegend', '展示图例', TRUE)),
                                column(4, checkboxInput('tgshownumber', '展示象限统计数字', TRUE))
                              ),
                              # colourInput("tgtextcol", "文本颜色", "black"),
                              sliderInput("tgtextsize", "文本大小", min = 1, max = 18, value = 14),
                              textInput("tgtitle", "设置title", value = "R1vsR2 ∩ R1vsR3"),
                              textInput("tgxlab", "设置x轴标签", value = "Log2FoldChange_R1vsR2"),
                              textInput("tgylab", "设置y轴标签", value = "Log2FoldChange_R1vsR3")
                            ),
                            
                            # 主控件
                            sliderInput("volcano_plot_panel_height", "展示区域高度", min = 100, max = 720, value = 500),
                            sliderInput("volcano_plot_panel_width", "展示区域宽度", min = 100, max = 720, value = 600),
                            fluidRow(
                              column(4, textInput("plot1width", "图片宽", value = 7)),
                              column(4, textInput("plot1height", "图片高", value = 5))
                            ),
                            radioButtons('extPlot', '设置图片输出格式', 
                                         choices = c('PDF' = 'pdf', "PNG" = 'png', 'JPEG' = 'jpeg'), 
                                         inline = TRUE),
                            downloadButton("plot1downloadData", "Download Plot",style = "color: #1abc9c; background-color: white; border-color: #27ae60;")
                          ),
                          cellWidths = c("70%", "30%")  # 调整为30%以更好显示控件
                        )
                      )
                    )
                  )
)

######富集分析布局
page4 <- tabPanel("GO/KEGG富集分析",icon = icon("circle-nodes",style="color: #74C0FC;"),
                  sidebarLayout(
                    sidebarPanel(width = 2,
                                 # style = "width: 20vw;margin-right: 0;",
                                 selectInput("species", "选择物种:",
                                             choices = c("拟南芥" = "athaliana",
                                                         "水稻" = "oryza",
                                                         "其他非模式物种" = "other")),
                                 conditionalPanel(
                                   condition = "input.species == 'other'",
                                   fileInput("Gene2Term2Name",
                                             label = "选择背景文件：geneid GO Description",
                                             accept = ".txt",
                                             placeholder = "请选择txt文件")),
                                 textAreaInput("enrich_gene_list", "输入基因列表 (每行一个基因):", 
                                               rows = 10, 
                                               placeholder = "AT1G01010\nAT1G01020\n...\nor\nLOC_Os01g01010\nLOC_Os02g12340"),
                                 radioButtons("analysis_type", "分析类型:",
                                              choices = c("GO富集" = "go","KEGG富集" = "kegg")),
                                 conditionalPanel(condition = "input.analysis_type == 'go'",
                                                  selectInput("go_ontology", "GO类别:",
                                                              choices = c("生物过程(BP)" = "BP",
                                                                          "分子功能(MF)" = "MF",
                                                                          "细胞组分(CC)" = "CC",
                                                                          "全部(ALL)" = "ALL"),
                                                              selected = c("全部(ALL)" = "ALL")
                                                              )),
                                 
                                 numericInput("pvalue_cutoff", "p值阈值:", value = 0.05, min = 0, max = 1, step = 0.01),
                                 numericInput("qvalue_cutoff", "q值阈值:", value = 0.2, min = 0, max = 1, step = 0.01),
                                 useShinyjs(),
                                 fluidRow(column(6,actionButton("run_enrich_analysis", "执行分析",icon = icon("play"),style = "color: white; background-color: #3498db; border-color: #2980b9;")),
                                          column(6,actionButton("page4_clear_btn", "清空输入", icon = icon("trash"),style = "color: white; background-color: #e74c3c; border-color: #c0392b;"))
                                 ),
                                 br(), br(),
                                 downloadButton("download_enrich_results", "下载富集结果",style = "color: white; background-color: #1abc9c; border-color: #16a085;"),
                                 ),
                    mainPanel(width = 10,
                              # style = "padding-left: 0; margin-left: -100px;",
                              add_busy_bar(color = "#74C0FC",height = "10px"),
                              fluidPage(
                                actionLink("page4_showexampledata","点击查看其他非模式物种所需输入背景文件格式"),
                                br(),
                                tabsetPanel(
                                  tabPanel("富集结果", 
                                           helpText("结果展示"),
                                           DTOutput("enrichment_table")),
                                  tabPanel("可视化", 
                                           # style = "height: 120vh; overflow-y: auto;",
                                           splitLayout(
                                             div(
                                               # style = "height: 90vh; overflow-y: auto;",
                                               style = "overflow-y: auto;",
                                               helpText("结果展示"),
                                               plotOutput("enrichment_plot",height = "100%")
                                               ),
                                             
                                             div(
                                               selectInput("enrich_plot_type", "选择图片展示方式",
                                                           choices = c("Barplot" = "barplot",
                                                                       "Dotplot" = "dotplot", 
                                                                       "GO关系图(igraph)" = "goplot",
                                                                       "emapplot" = "emapplot",
                                                                       "cnetplot" = "cnetplot"
                                                                       )),
                                               sliderInput("show_category", "显示条目数:", 
                                                           min = 1, max = 30, value = 5),
                                               conditionalPanel(
                                                 condition = "input.enrich_plot_type == 'cnetplot'",
                                                 checkboxInput('cnetplotcricos', '是否环形展示', TRUE)
                                                 ),
                                               sliderInput("enrichplotfontsize","设置文本大小",min = 1,max = 20,value = 12),
                                               textInput("enrichplottitle","设置你的图片title",value = "enrich Plot"),
                                               sliderInput("enrich_plot_panel_height", "展示区域高度", min = 100, max = 1080, value = 720),
                                               sliderInput("enrich_plot_panel_width", "展示区域宽度", min = 100, max = 1080, value = 720),
                                               textInput("enrichplotwidth","图片输出宽度",value = 5),
                                               textInput("enrichplotheight","图片输出高度",value = 7),
                                               radioButtons('extenrichPlot', '图片输出格式(推荐pdf)',choices = c('PDF'='pdf',"PNG"='png', 'JPEG'='jpeg'), inline = T),
                                               downloadButton("enrichplotdownload","下载图片",style = "color: white; background-color: #1abc9c; border-color: #16a085;")
                                               ),
                                             cellWidths = c("80%", "20%")
                                             )
                                           )
                                  )
                                )
                              )
                    )
                  )
######富集高级可视化
page5 <- tabPanel("富集分析高级绘图(测试)",icon = icon("wand-magic-sparkles",class = "fa-solid fa-wand-magic-sparkles", style = "color: #74C0FC;") , # 使用Font Awesome图标
                  sidebarLayout(
                    sidebarPanel(width = 3,
                                 fileInput("page5_EnrichFile", "选择富集结果文件"),
                                 helpText("只支持clusterProfiler结果格式文件"),
                                 hr(),
                                 checkboxInput('page5_header', '第一行作为表头', TRUE),
                                 radioButtons('page5_sep', '数据分隔符号', 
                                              c(Tab = '\t', Comma = ',', Semicolon = ';'), 
                                              selected = '\t', inline = TRUE),
                                 hr(),
                                 hr(),
                                 selectInput("page5_enrichplottype","选择绘图类型",
                                             choices = c("气泡图" = "page5_bubbleplot",
                                                         "横向Barplot" = "page5_horizontal_barplot",
                                                         "纵向Barplot" = "page5_vertical_barplot",
                                                         "富集聚类图" = "page5_cluster_enrichplot",
                                                         "环型富集图(未完成)" = "page5_circlize_enrichplot"
                                             )),
                                 # 气泡图输入参数
                                 conditionalPanel(condition = "input.page5_enrichplottype == 'page5_bubbleplot'",
                                                  bsCollapse(
                                                    id = "page5_bubbleplot_data_process",
                                                    open = "bubble_data_process",  # 默认关闭
                                                    bsCollapsePanel(
                                                      title = "气泡图数据预处理",
                                                      value = "bubble_data_process",
                                                      style = "info",
                                                      # numericInput("page5_bubbleplot_Description_warp", "Description列换行最大字符数", value = 40),
                                                      checkboxInput('page5_bubbleplot_data_order',"是否对数据按某一列排序",TRUE),
                                                      helpText('不排序的情况下默认按照Description列原生顺序排列'),
                                                      conditionalPanel(condition = "input.page5_bubbleplot_data_order == true",
                                                                       textInput("page5_bubbleplot_data_order_by","选择按照哪一列排序",value = 'RichFactor'),
                                                                       checkboxInput("page5_bubbleplot_data_order_desc","降序排列",TRUE)
                                                      ) 
                                                    )
                                                  )
                                 ),
                                 # 横向Barplot输入参数
                                 conditionalPanel(condition = "input.page5_enrichplottype == 'page5_horizontal_barplot'",
                                                  bsCollapse(
                                                    id = "page5_horizontal_barplot_data_process",
                                                    open = "horizontal_bar_data_process",  # 默认关闭
                                                    bsCollapsePanel(
                                                      title = "横向Barplot数据预处理",
                                                      value = "horizontal_bar_data_process",
                                                      style = "info",
                                                      # numericInput("page5_horizontal_barplot_Description_warp", "Description列换行最大字符数", value = 200),
                                                      checkboxInput('page5_horizontal_barplot_data_order',"是否对数据按某一列排序",TRUE),
                                                      helpText('不排序的情况下默认按照Description列原生顺序排列'),
                                                      conditionalPanel(condition = "input.page5_horizontal_barplot_data_order == true",
                                                                       textInput("page5_horizontal_barplot_data_order_by","选择按照哪一列排序",value = 'pvalue'),
                                                                       checkboxInput("page5_horizontal_barplot_data_order_desc","降序排列",TRUE)
                                                      ) 
                                                    )
                                                  )
                                 ),
                                 # 纵向barplot输入参数
                                 conditionalPanel(condition = "input.page5_enrichplottype == 'page5_vertical_barplot'",
                                                  helpText("纵向Barplot数据预处理"),
                                                  bsCollapse(
                                                    id = "page5_vertical_barplot_data_process",
                                                    open = "vertical_bar_data_process",  # 默认关闭
                                                    bsCollapsePanel(
                                                      title = "纵向Barplot数据预处理",
                                                      value = "vertical_bar_data_process",
                                                      style = "info",
                                                      checkboxInput('page5_vertical_barplot_data_order',"是否对数据按某一列排序",TRUE),
                                                      helpText('不排序的情况下默认按照Description列原生顺序排列'),
                                                      conditionalPanel(condition = "input.page5_vertical_barplot_data_order == true",
                                                                       textInput("page5_vertical_barplot_data_order_by","选择按照哪一列排序",value = 'RichFactor'),
                                                                       checkboxInput("page5_vertical_barplot_data_order_desc","降序排列",TRUE)
                                                      ) 
                                                    )
                                                  )
                                 ),
                                 # 环形富集图输入参数
                                 conditionalPanel(condition = "input.page5_enrichplottype == 'page5_circlize_enrichplot'",
                                                  helpText("环型富集图数据预处理")
                                 ),
                                 # 富集聚类图输入参数
                                 conditionalPanel(condition = "input.page5_enrichplottype == 'page5_cluster_enrichplot'",
                                                  helpText("富集聚类图数据预处理")
                                 ),
                                 
                                 useShinyjs(),
                                 fluidRow(column(6,actionButton("page5_plotstart", "开始绘图",icon = icon("play"),style = "color: white; background-color: #3498db; border-color: #2980b9;")),
                                          column(6,actionButton("page5_clear_btn", "清空输入", icon = icon("trash"),style = "color: white; background-color: #e74c3c; border-color: #c0392b;"))
                                 )
                    ),
                    mainPanel(width = 9,
                              fluidPage(
                                # width = 12,
                                style = "padding-left: 0; margin-left: 0px;padding-right: 0; margin-right: 0px;",
                                splitLayout(
                                  ## 主界面
                                  div(
                                    actionLink("page5_showexampledata","点击查看富集分析输入数据格式"),
                                    helpText("绘图展示"),
                                    plotOutput("page5_plot", height = "80%"),
                                    helpText("数据展示"),
                                    div(
                                      style = "overflow-y: auto;overflow-x: auto;",
                                      DT::DTOutput("page5_text")
                                    ) 
                                  ),
                                  
                                  ## 右面控制面板 
                                  div(
                                    ########--------bubbleplot面板--------########
                                    conditionalPanel(
                                      condition = "input.page5_enrichplottype == 'page5_bubbleplot'",
                                      ## 添加一个选择折叠框
                                      bsCollapse(
                                        id = "page5_bubbleplot",
                                        open = "bubble_framework_settings",  # 默认关闭
                                        bsCollapsePanel(
                                          title = "绘图框架设置",
                                          value = "bubble_framework_settings",
                                          style = "warning",
                                          helpText("设置气泡图基础框架"),
                                          fluidRow(
                                            column(4, textInput("page5_bubbleplot_x", "X轴", value = "RichFactor")),
                                            column(4, textInput("page5_bubbleplot_y", "Y轴", value = "Description")),
                                            column(4, textInput("page5_bubbleplot_fill", "气泡颜色填充映射", value = "pvalue"))
                                          ),
                                          fluidRow(
                                            column(4, textInput("page5_bubbleplot_size", "气泡大小映射", value = "Count")),
                                            column(4, numericInput("page5_bubbleplot_Description_warp", "换行最大字符数", value = 40)),
                                            column(4,selectInput("page5_bubbleplot_show_legend","展示图例",
                                                                 choices = c("展示" = TRUE,
                                                                             "不展示" = FALSE
                                                                 )))
                                          ),
                                          helpText("设置气泡图标签"),
                                          fluidRow(
                                            column(6, textInput("page5_bubbleplot_title", "主标题", value = "Enrich Bubble Plot")),
                                            column(6, textInput("page5_bubbleplot_subtitle","次标题",value = "Upgrade Genes GO Enrich Result"))
                                          )
                                        ),
                                        bsCollapsePanel(
                                          title = "气泡参数设置",
                                          value = "bubble_bubble_settings",
                                          style = "warning",
                                          fluidRow(
                                            column(12, sliderInput("page5_bubbleplot_bubble_alpha", "气泡透明度", min = 0.1, max = 1, value = 1))
                                          ),
                                          fluidRow(
                                            column(12, sliderInput("page5_bubbleplot_bubble_stroke", "气泡边框厚度",min = 0, max = 5, value = 1))
                                          ),
                                          fluidRow(
                                            column(12, sliderInput("page5_bubbleplot_bubble_shape", "气泡形状类型",min = 21, max = 25, value = 21))
                                          ),
                                          fluidRow(
                                            column(6, colourInput("page5_bubbleplot_bubble_colour", "气泡边框颜色","white")),
                                            column(6, colourInput("page5_bubbleplot_legend_bubble_colour", "图例气泡颜色","grey20"))
                                          ),
                                          fluidRow(
                                            column(6, colourInput("page5_bubbleplot_fill_height_colour", "气泡填充高值颜色","red")),
                                            column(6, colourInput("page5_bubbleplot_fill_low_colour", "气泡填充低值颜色","blue"))
                                          )
                                        ),
                                        bsCollapsePanel(
                                          title = "Theme参数设置",
                                          value = "bubble_Theme_settings",
                                          style = "warning",
                                          helpText("框线设置"),
                                          fluidRow(
                                            column(4, checkboxInput("page5_bubbleplot_panel.background", "空白背景", TRUE)),
                                            column(4, checkboxInput("page5_bubbleplot_panel.grid.major", "主网格线", FALSE)),
                                            column(4, checkboxInput("page5_bubbleplot_panel.grid.minor", "次网格线", FALSE))
                                          ),
                                          hr(),
                                          helpText("文本设置"),
                                          fluidRow(
                                            column(6, colourInput("page5_bubbleplot_axis.text_colour","轴标签文本颜色","black")),
                                            column(6, selectInput("page5_bubbleplot_axis.text_Font","轴标签字体",
                                                                  choices = c("普通" = "plain",
                                                                              "加粗" = 'bold',
                                                                              "斜体" = 'italic',
                                                                              "加粗斜体" = 'bold.italic'
                                                                  )))
                                          ),
                                          sliderInput("page5_bubbleplot_axis.text_size","轴标签字号",min = 1, max = 20, value = 12),
                                          hr(),
                                          helpText("X,Y,legend标题设置"),
                                          fluidRow(
                                            column(6, colourInput("page5_bubbleplot_axis.title_colour","X,Y,legend标题颜色","black")),
                                            column(6, selectInput("page5_bubbleplot_axis.title_Font","X,Y,legend标题字体",
                                                                  choices = c("普通" = "plain",
                                                                              "加粗" = 'bold',
                                                                              "斜体" = 'italic',
                                                                              "加粗斜体" = 'bold.italic'
                                                                  )))
                                          ),
                                          sliderInput("page5_bubbleplot_axis.title_size","X,Y,legend标签大小",min = 1, max = 20, value = 13),
                                          hr(),
                                          helpText("主标题设置"),
                                          fluidRow(
                                            column(4, colourInput("page5_bubbleplot_plot.title_colour","主标题颜色","black")),
                                            column(4, selectInput("page5_bubbleplot_plot.title_Font","主标题字体",
                                                                  choices = c("普通" = "plain",
                                                                              "加粗" = 'bold',
                                                                              "斜体" = 'italic',
                                                                              "加粗斜体" = 'bold.italic'
                                                                  )))
                                          ),
                                          sliderInput("page5_bubbleplot_plot.title_size","主标题大小",min = 1, max = 20, value = 15),
                                          hr(),
                                          helpText("次标题设置"),
                                          fluidRow(
                                            column(4, colourInput("page5_bubbleplot_plot.subtitle_colour","次标题颜色","black")),
                                            column(4, selectInput("page5_bubbleplot_plot.subtitle_Font","次标题字体",
                                                                  choices = c("斜体" = 'italic',
                                                                              "加粗" = 'bold',
                                                                              "加粗斜体" = 'bold.italic',
                                                                              "普通" = "plain"
                                                                  )
                                            ))
                                          ),
                                          sliderInput("page5_bubbleplot_plot.subtitle_size","次标题大小",min = 1, max = 20, value = 13),
                                          hr(),
                                          helpText("边框设置"),
                                          colourInput("page5_bubbleplot_panel.border_colour","边框颜色","black"),
                                          sliderInput("page5_bubbleplot_panel.border_size","边框宽度",min = 0.1, max = 2, value = 0.5)
                                        ),
                                        bsCollapsePanel(
                                          title = "分面参数设置",
                                          value = "bubble_facet_settings",
                                          style = "warning",
                                          helpText("设置"),
                                          selectInput("page5_bubbleplot_facet","是否分面",
                                                      choices = c("分面" = TRUE,
                                                                  "不分面" = FALSE
                                                      ),selected = c("不分面" = FALSE)),
                                          conditionalPanel(condition = "input.page5_bubbleplot_facet == 'TRUE'",
                                                           textInput("page5_bubbleplot_facet_by", "按哪一列分面", value = "ONTOLOGY"),
                                                           helpText("背景颜色"),
                                                           fluidRow(
                                                             column(4,colourInput("page5_bubbleplot_facet_colour1","分面1颜色","grey")),
                                                             column(4,colourInput("page5_bubbleplot_facet_colour2","分面2颜色","grey")),
                                                             column(4,colourInput("page5_bubbleplot_facet_colour3","分面3颜色","grey"))
                                                           ),
                                                           helpText("字体字号"),
                                                           fluidRow(
                                                             column(12,selectInput("page5_bubbleplot_facet_Font","分面字体",
                                                                                   choices = c("普通" = "plain",
                                                                                               "加粗" = 'bold',
                                                                                               "斜体" = 'italic',
                                                                                               "加粗斜体" = 'bold.italic'
                                                                                   )))
                                                           ),
                                                           fluidRow(
                                                             column(12, sliderInput("page5_bubbleplot_facet_size", "分面字号",min = 5, max = 20, value = 13))
                                                           ))  
                                        )
                                      )
                                    ),
                                    ########--------horizontal_barplot面板--------########
                                    conditionalPanel(
                                      condition = "input.page5_enrichplottype == 'page5_horizontal_barplot'",
                                      ## 添加一个选择折叠框
                                      bsCollapse(
                                        id = "page5_horizontal_barplot",
                                        open = "horizontal_bar_framework_settings",  # 默认关闭
                                        bsCollapsePanel(
                                          title = "绘图框架设置",
                                          value = "horizontal_bar_framework_settings",
                                          style = "warning",
                                          helpText("设置横向柱形图基础框架"),
                                          fluidRow(
                                            column(4, textInput("page5_horizontal_barplot_x", "X轴", value = "pvalue")),
                                            column(4, textInput("page5_horizontal_barplot_y", "Y轴", value = "Description")),
                                            column(4, numericInput("page5_horizontal_barplot_Description_warp", "换行最大字符数", value = 200))
                                          ),
                                          fluidRow(
                                            column(6, selectInput("page5_horizontal_barplot_show_legend","展示图例",
                                                                  choices = c("展示" = TRUE,
                                                                              "不展示" = FALSE
                                                                  ))),
                                            column(6,selectInput("page5_horizontal_barplot_use_fill","使用填充颜色映射",
                                                                 choices = c("使用" = TRUE,
                                                                             "不使用" = FALSE
                                                                 )))
                                          ),
                                          conditionalPanel(condition = "input.page5_horizontal_barplot_use_fill == 'TRUE'",
                                                           textInput("page5_horizontal_barplot_fill", "Bar颜色填充映射", value = "pvalue")),
                                          conditionalPanel(condition = "input.page5_horizontal_barplot_use_fill == 'FALSE'",
                                                           colourInput("page5_horizontal_barplot_fill_single_colour", "Bar颜色", "grey")),
                                          hr(),
                                          helpText("设置柱形图标签"),
                                          fluidRow(
                                            column(6, textInput("page5_horizontal_barplot_title", "主标题", value = "GO Enrich Barplot")),
                                            column(6, textInput("page5_horizontal_barplot_subtitle","次标题",value = "Enriched ONTOLOPGY of Top 760 Upgrade genes"))
                                          ),
                                          fluidRow(
                                            column(6, textInput("page5_horizontal_barplot_labx", "X轴标签", value = "-log10(pvalue)")),
                                            column(6, textInput("page5_horizontal_barplot_laby","Y轴标签",value = "GO Term Description"))
                                          )
                                        ),
                                        bsCollapsePanel(
                                          title = "Bar参数设置",
                                          value = "horizontal_bar_settings",
                                          style = "warning",
                                          helpText("Bar参数"),
                                          fluidRow(
                                            column(12, sliderInput("page5_horizontal_barplot_bar_alpha", "Bar透明度", min = 0.1, max = 1, value = 1))
                                          ),
                                          fluidRow(
                                            column(12, sliderInput("page5_horizontal_barplot_bar_linewidth", "Bar边框厚度",min = 0, max = 2, value = 0))
                                          ),
                                          fluidRow(
                                            column(12, sliderInput("page5_horizontal_barplot_bar_width", "Bar宽度",min = 0.1, max = 1, value = 0.5))
                                          ),
                                          fluidRow(
                                            column(12, sliderInput("page5_horizontal_barplot_bar_just", "Bar垂直移动",min = 0, max = 1, value = 0.5 ,step =0.1))
                                          ),
                                          fluidRow(
                                            conditionalPanel(
                                              condition = "input.page5_horizontal_barplot_use_fill == 'TRUE'",
                                              column(6, selectInput("page5_horizontal_barplot_barfill_use_palette","填充颜色:配色板 or 手动",
                                                                    choices = c("配色板" = TRUE,
                                                                                "手动" = FALSE
                                                                    )))),
                                            column(6, colourInput("page5_horizontal_barplot_bar_colour", "Bar边框颜色","white"))
                                            
                                          ),
                                          ## 连续变量使用配色板
                                          conditionalPanel(
                                            condition = "input.page5_horizontal_barplot_use_fill == 'TRUE' && input.page5_horizontal_barplot_barfill_use_palette == 'TRUE'",
                                            fluidRow(
                                              column(6, selectInput("page5_horizontal_barplot_barfill_palette","配色板",
                                                                    choices = c("蓝渐变" = "Blues",
                                                                                "绿渐变" = "Greens",
                                                                                "灰渐变" = "Greys",
                                                                                "橙渐变" = "Oranges",
                                                                                "紫渐变" = "Purples",
                                                                                "红渐变" = "Reds",
                                                                                "蓝绿渐变" = "BuGn",
                                                                                "绿紫渐变" = "BuPu",
                                                                                "绿蓝渐变" = "GnBu",
                                                                                "橙红渐变" = "OrRd",
                                                                                "紫蓝渐变" = "PuBu",
                                                                                "紫红渐变" = "PuRd",
                                                                                "红紫渐变" = "RdPu",
                                                                                "黄绿渐变" = "YlGn",
                                                                                "紫蓝绿渐变" = "PuBuGn",
                                                                                "黄绿蓝渐变" = "YlGnBu",
                                                                                "黄橙棕渐变" = "YlOrBr",
                                                                                "黄橙红渐变" = "YlOrRd",
                                                                                "棕绿离散" = "BrBG",
                                                                                "粉绿离散" = "PiYG",
                                                                                "紫绿离散" = "PRGn",
                                                                                "紫棕离散" = "PuOr",
                                                                                "红蓝离散" = "RdBu",
                                                                                "红灰离散" = "RdGy",
                                                                                "红黄蓝离散" = "RdYlBu",
                                                                                "红黄绿离散" = "RdYlGn",
                                                                                "光谱离散" = "Spectral"
                                                                    ))),
                                              column(6, selectInput("page5_horizontal_barplot_barfill_palette_direction", "颜色反向",
                                                                    choices = c("原向" = 1,
                                                                                "反向" = -1)
                                              ))
                                            )
                                          ),
                                          
                                          ## 分类变量手动配色
                                          conditionalPanel(
                                            condition = "input.page5_horizontal_barplot_use_fill == 'TRUE' && input.page5_horizontal_barplot_barfill_use_palette == 'FALSE'",
                                            helpText(HTML("对于连续性变量，可以直接使用<br/>'Bar填充映射颜色1'与'Bar填充映射颜色2'<br/>作为<高值>与<低值>的颜色")),
                                            fluidRow(
                                              column(6, colourInput("page5_horizontal_barplot_fill_colour1", "Bar填充映射颜色1","#1F78B4")),
                                              column(6, colourInput("page5_horizontal_barplot_fill_colour2", "Bar填充映射颜色2","#FFB300"))
                                            ),
                                            fluidRow(
                                              column(6, colourInput("page5_horizontal_barplot_fill_colour3", "Bar填充映射颜色3","#FA2017")),
                                              column(6, colourInput("page5_horizontal_barplot_fill_colour4", "Bar填充映射颜色4","#33A02C"))
                                            ),
                                          ),
                                          
                                          
                                          
                                          
                                          hr(),
                                          helpText("Bar上标签设置"),
                                          fluidRow(
                                            column(6, colourInput("page5_horizontal_barplot_geom_text1_colour","Bar标签颜色","black")),
                                            column(6, selectInput("page5_horizontal_barplot_geom_text1_Font","Bar标签字体",
                                                                  choices = c("普通" = "plain",
                                                                              "加粗" = 'bold',
                                                                              "斜体" = 'italic',
                                                                              "加粗斜体" = 'bold.italic'),selected = c("加粗" = 'bold')))
                                          ),
                                          sliderInput("page5_horizontal_barplot_geom_text1_size","Bar标签字号",min = 1, max = 12, value = 5,step = 0.5),
                                          sliderInput("page5_horizontal_barplot_geom_text1_hjust","Bar标签水平移动",min = -1, max = 1, value = 0,step = 0.1),
                                          sliderInput("page5_horizontal_barplot_geom_text1_vjust","Bar标签垂直移动",min = -2, max = 2, value = 0.5,step = 0.1),
                                          sliderInput("page5_horizontal_barplot_geom_text1_x","Bar标签x轴起点",min = 0.001, max = 0.2, value = 0.1)
                                        ),
                                        bsCollapsePanel(
                                          title = "GeneID参数设置",
                                          value = "horizontal_geneid_settings",
                                          style = "warning",
                                          helpText("是否展示GeneID"),
                                          fluidRow(
                                            column(6,selectInput("page5_horizontal_barplot_show_geneid","展示基因ID",
                                                                 choices = c("展示" = TRUE,
                                                                             "不展示" = FALSE
                                                                 ))),
                                            column(6,conditionalPanel(condition = "input.page5_horizontal_barplot_show_geneid == 'TRUE'", 
                                                                      textInput("page5_horizontal_barplot_geneid_col", "ID所在列", value = "geneID")))
                                          ),
                                          conditionalPanel(condition = "input.page5_horizontal_barplot_show_geneid == 'TRUE'", 
                                                           fluidRow(
                                                             column(6,selectInput("page5_horizontal_barplot_geom_text2_colour_by_value","ID文字颜色固定 or 映射",
                                                                                  choices = c("固定色" = FALSE,
                                                                                              "映射" = TRUE
                                                                                  ))),
                                                             column(6,conditionalPanel(condition = "input.page5_horizontal_barplot_geom_text2_colour_by_value == 'TRUE'", 
                                                                                       textInput("page5_horizontal_barplot_geom_text2_colour_by", "ID文字颜色填充映射", value = "ONTOLOGY"))
                                                             ))
                                          ),
                                          conditionalPanel(condition = "input.page5_horizontal_barplot_show_geneid == 'TRUE'",
                                                           hr(),
                                                           helpText("GeneID设置"),
                                                           conditionalPanel(
                                                             condition = "input.page5_horizontal_barplot_geom_text2_colour_by_value == 'TRUE'",
                                                             fluidRow(
                                                               column(6, colourInput("page5_horizontal_barplot_geom_text2_colour1", "GeneID映射颜色1","#1F78B4")),
                                                               column(6, colourInput("page5_horizontal_barplot_geom_text2_colour2", "GeneID映射颜色2","#FFB300"))
                                                             ),
                                                             fluidRow(
                                                               column(6, colourInput("page5_horizontal_barplot_geom_text2_colour3", "GeneID映射颜色3","#FA2017")),
                                                               column(6, colourInput("page5_horizontal_barplot_geom_text2_colour4", "GeneID映射颜色4","#33A02C"))
                                                             ),
                                                           ),
                                                           fluidRow(
                                                             column(6, selectInput("page5_horizontal_barplot_geom_text2_Font","GeneID标签字体",
                                                                                   choices = c("普通" = "plain",
                                                                                               "加粗" = 'bold',
                                                                                               "斜体" = 'italic',
                                                                                               "加粗斜体" = 'bold.italic'),selected = c("加粗" = 'bold'))),
                                                             column(6, conditionalPanel(
                                                               condition = "input.page5_horizontal_barplot_geom_text2_colour_by_value == 'FALSE'",
                                                               colourInput("page5_horizontal_barplot_geom_text2_colour","GeneID标签颜色","black"))
                                                             )
                                                           ),
                                                           sliderInput("page5_horizontal_barplot_geom_text2_size","GeneID标签字号",min = 1, max = 10, value = 3.5,step = 0.5),
                                                           sliderInput("page5_horizontal_barplot_geom_text2_hjust","GeneID标签水平移动",min = -1, max = 1, value = 0,step = 0.1),
                                                           sliderInput("page5_horizontal_barplot_geom_text2_vjust","GeneID标签垂直移动",min = -3, max = 3, value = 2.3,step = 0.1),
                                                           sliderInput("page5_horizontal_barplot_geom_text2_x","GeneID标签x轴起点",min = 0.001, max = 0.2, value = 0.1)
                                          )
                                        ),
                                        bsCollapsePanel(
                                          title = "Theme参数设置",
                                          value = "horizontal_bar_Theme_settings",
                                          style = "warning",
                                          helpText("边框设置"),
                                          colourInput("page5_horizontal_barplot_panel.border_colour","边框颜色","black"),
                                          sliderInput("page5_horizontal_barplot_panel.border_linewidth","边框宽度",min = 0.1, max = 2, value = 1.5,step = 0.1),
                                          hr(),
                                          helpText("文本设置"),
                                          fluidRow(
                                            column(6, colourInput("page5_horizontal_barplot_axis.text_colour","轴标签文本颜色","black")),
                                            column(6, selectInput("page5_horizontal_barplot_axis.text_Font","轴标签字体",
                                                                  choices = c("普通" = "plain",
                                                                              "加粗" = 'bold',
                                                                              "斜体" = 'italic',
                                                                              "加粗斜体" = 'bold.italic'
                                                                  )))
                                          ),
                                          sliderInput("page5_horizontal_barplot_axis.text_size","轴标签字号",min = 1, max = 20, value = 12),
                                          hr(),
                                          helpText("X,Y,legend标题设置"),
                                          fluidRow(
                                            column(6, colourInput("page5_horizontal_barplot_axis.title_colour","X,Y,legend标题颜色","black")),
                                            column(6, selectInput("page5_horizontal_barplot_axis.title_Font","X,Y,legend标题字体",
                                                                  choices = c("普通" = "plain",
                                                                              "加粗" = 'bold',
                                                                              "斜体" = 'italic',
                                                                              "加粗斜体" = 'bold.italic'
                                                                  ),selected = c("加粗" = 'bold')))
                                          ),
                                          sliderInput("page5_horizontal_barplot_axis.title_size","X,Y,legend标签大小",min = 1, max = 20, value = 13),
                                          hr(),
                                          helpText("主标题设置"),
                                          fluidRow(
                                            column(4, colourInput("page5_horizontal_barplot_plot.title_colour","主标题颜色","black")),
                                            column(4, selectInput("page5_horizontal_barplot_plot.title_Font","主标题字体",
                                                                  choices = c("普通" = "plain",
                                                                              "加粗" = 'bold',
                                                                              "斜体" = 'italic',
                                                                              "加粗斜体" = 'bold.italic'
                                                                  ),selected = c("加粗" = 'bold')))
                                          ),
                                          sliderInput("page5_horizontal_barplot_plot.title_hjust","主标题水平移动",min = -1, max = 1, value = 0,step = 0.1),
                                          sliderInput("page5_horizontal_barplot_plot.title_size","主标题大小",min = 1, max = 20, value = 15),
                                          hr(),
                                          helpText("次标题设置"),
                                          fluidRow(
                                            column(4, colourInput("page5_horizontal_barplot_plot.subtitle_colour","次标题颜色","black")),
                                            column(4, selectInput("page5_horizontal_barplot_plot.subtitle_Font","次标题字体",
                                                                  choices = c("斜体" = 'italic',
                                                                              "加粗" = 'bold',
                                                                              "加粗斜体" = 'bold.italic',
                                                                              "普通" = "plain"
                                                                  ),selected = c("斜体" = 'italic')
                                            ))
                                          ),
                                          sliderInput("page5_horizontal_barplot_plot.subtitle_hjust","次标题水平移动",min = -1, max = 1, value = 0,step = 0.1),
                                          sliderInput("page5_horizontal_barplot_plot.subtitle_size","次标题大小",min = 1, max = 20, value = 13)
                                          
                                        ),
                                        bsCollapsePanel(
                                          title = "分面参数设置",
                                          value = "horizontal_bar_facet_settings",
                                          style = "warning",
                                          helpText("设置"),
                                          selectInput("page5_horizontal_barplot_facet","是否分面",
                                                      choices = c("分面" = TRUE,
                                                                  "不分面" = FALSE
                                                      ),selected = c("不分面" = FALSE)),
                                          conditionalPanel(
                                            condition = "input.page5_horizontal_barplot_facet == 'TRUE'",
                                            textInput("page5_horizontal_barplot_facet_by", "按哪一列分面", value = "ONTOLOGY"),
                                            helpText("分面标签背景颜色"),
                                            fluidRow(
                                              column(6,colourInput("page5_horizontal_barplot_facet_fill_colour1","分面1背景颜色","#1F78B4")),
                                              column(6,colourInput("page5_horizontal_barplot_facet_fill_colour2","分面2背景颜色","#FFB300"))
                                            ),
                                            fluidRow(
                                              column(6,colourInput("page5_horizontal_barplot_facet_fill_colour3","分面3背景颜色","#FA2017")),
                                              column(6,colourInput("page5_horizontal_barplot_facet_fill_colour4","分面4背景颜色","#33A02C"))
                                            ),
                                            helpText("分面标签边框颜色"),
                                            fluidRow(
                                              column(6,colourInput("page5_horizontal_barplot_facet_border_colour1","分面1边框颜色","#1F78B4")),
                                              column(6,colourInput("page5_horizontal_barplot_facet_border_colour2","分面2边框颜色","#FFB300"))
                                            ),
                                            fluidRow(
                                              column(6,colourInput("page5_horizontal_barplot_facet_border_colour3","分面3边框颜色","#FA2017")),
                                              column(6,colourInput("page5_horizontal_barplot_facet_border_colour4","分面4边框颜色","#33A02C"))
                                            ),
                                            helpText("字体字号"),
                                            fluidRow(
                                              column(12,selectInput("page5_horizontal_barplot_facet_Font","分面字体",
                                                                    choices = c("普通" = "plain",
                                                                                "加粗" = 'bold',
                                                                                "斜体" = 'italic',
                                                                                "加粗斜体" = 'bold.italic'
                                                                    )))
                                            ),
                                            fluidRow(
                                              column(12, sliderInput("page5_horizontal_barplot_facet_size", "分面字号",min = 5, max = 20, value = 10))
                                            )
                                          )  
                                        )
                                      )
                                    ),
                                    ########--------vertical_barplot面板--------########
                                    conditionalPanel(
                                      condition = "input.page5_enrichplottype == 'page5_vertical_barplot'",
                                      ## 添加一个选择折叠框
                                      bsCollapse(
                                        id = "page5_vertical_barplot",
                                        open = "vertical_bar_framework_settings",  # 默认关闭
                                        bsCollapsePanel(
                                          title = "绘图框架设置",
                                          value = "vertical_bar_framework_settings",
                                          style = "warning",
                                          helpText("设置横向柱形图基础框架"),
                                          fluidRow(
                                            column(4, textInput("page5_vertical_barplot_x", "X轴", value = "Description")),
                                            column(4, textInput("page5_vertical_barplot_y", "Y轴", value = "RichFactor")),
                                            column(4, numericInput("page5_vertical_barplot_Description_warp", "换行最大字符数", value = 40))
                                          ),
                                          fluidRow(
                                            column(6, selectInput("page5_vertical_barplot_show_legend","展示图例",
                                                                  choices = c("展示" = TRUE,
                                                                              "不展示" = FALSE
                                                                  ))),
                                            column(6,selectInput("page5_vertical_barplot_use_fill","使用填充颜色映射",
                                                                 choices = c("使用" = TRUE,
                                                                             "不使用" = FALSE
                                                                 )))
                                          ),
                                          conditionalPanel(condition = "input.page5_vertical_barplot_use_fill == 'TRUE'",
                                                           textInput("page5_vertical_barplot_fill", "Bar颜色填充映射", value = "pvalue")),
                                          conditionalPanel(condition = "input.page5_vertical_barplot_use_fill == 'FALSE'",
                                                           colourInput("page5_vertical_barplot_fill_single_colour", "Bar颜色", "grey")),
                                          hr(),
                                          helpText("设置柱形图标签"),
                                          fluidRow(
                                            column(6, textInput("page5_vertical_barplot_title", "主标题", value = "GO Enrich Barplot")),
                                            column(6, textInput("page5_vertical_barplot_subtitle","次标题",value = "Enriched ONTOLOPGY of Top 760 Upgrade genes"))
                                          ),
                                          fluidRow(
                                            column(6, textInput("page5_vertical_barplot_labx", "X轴标签", value = "GO Term Description")),
                                            column(6, textInput("page5_vertical_barplot_laby","Y轴标签",value = "RichFactor"))
                                          )
                                        ),
                                        bsCollapsePanel(
                                          title = "Bar参数设置",
                                          value = "vertical_bar_settings",
                                          style = "warning",
                                          helpText("Bar参数"),
                                          fluidRow(
                                            column(12, sliderInput("page5_vertical_barplot_bar_alpha", "Bar透明度", min = 0.1, max = 1, value = 1))
                                          ),
                                          fluidRow(
                                            column(12, sliderInput("page5_vertical_barplot_bar_linewidth", "Bar边框厚度",min = 0, max = 2, value = 0))
                                          ),
                                          fluidRow(
                                            column(12, sliderInput("page5_vertical_barplot_bar_width", "Bar宽度",min = 0.1, max = 1, value = 0.8))
                                          ),
                                          fluidRow(
                                            column(12, sliderInput("page5_vertical_barplot_bar_hjust", "Bar水平移动",min = 0, max = 1, value = 0.5 ,step =0.1))
                                          ),
                                          fluidRow(
                                            conditionalPanel(
                                              condition = "input.page5_vertical_barplot_use_fill == 'TRUE'",
                                              column(6, selectInput("page5_vertical_barplot_barfill_use_palette","填充颜色:配色板 or 手动",
                                                                    choices = c("配色板" = TRUE,
                                                                                "手动" = FALSE
                                                                    )))),
                                            column(6, colourInput("page5_vertical_barplot_bar_colour", "Bar边框颜色","white"))
                                          ),
                                          ## 连续变量使用配色板
                                          conditionalPanel(
                                            condition = "input.page5_vertical_barplot_use_fill == 'TRUE' && input.page5_vertical_barplot_barfill_use_palette == 'TRUE'",
                                            fluidRow(
                                              column(6, selectInput("page5_vertical_barplot_barfill_palette","配色板",
                                                                    choices = c("蓝渐变" = "Blues",
                                                                                "绿渐变" = "Greens",
                                                                                "灰渐变" = "Greys",
                                                                                "橙渐变" = "Oranges",
                                                                                "紫渐变" = "Purples",
                                                                                "红渐变" = "Reds",
                                                                                "蓝绿渐变" = "BuGn",
                                                                                "绿紫渐变" = "BuPu",
                                                                                "绿蓝渐变" = "GnBu",
                                                                                "橙红渐变" = "OrRd",
                                                                                "紫蓝渐变" = "PuBu",
                                                                                "紫红渐变" = "PuRd",
                                                                                "红紫渐变" = "RdPu",
                                                                                "黄绿渐变" = "YlGn",
                                                                                "紫蓝绿渐变" = "PuBuGn",
                                                                                "黄绿蓝渐变" = "YlGnBu",
                                                                                "黄橙棕渐变" = "YlOrBr",
                                                                                "黄橙红渐变" = "YlOrRd",
                                                                                "棕绿离散" = "BrBG",
                                                                                "粉绿离散" = "PiYG",
                                                                                "紫绿离散" = "PRGn",
                                                                                "紫棕离散" = "PuOr",
                                                                                "红蓝离散" = "RdBu",
                                                                                "红灰离散" = "RdGy",
                                                                                "红黄蓝离散" = "RdYlBu",
                                                                                "红黄绿离散" = "RdYlGn",
                                                                                "光谱离散" = "Spectral"
                                                                    ))),
                                              column(6, selectInput("page5_vertical_barplot_barfill_palette_direction", "颜色反向",
                                                                    choices = c("原向" = 1,
                                                                                "反向" = -1)
                                              ))
                                            )
                                          ),
                                          
                                          ## 分类变量手动配色
                                          conditionalPanel(
                                            condition = "input.page5_vertical_barplot_use_fill == 'TRUE' && input.page5_vertical_barplot_barfill_use_palette == 'FALSE'",
                                            wellPanel(helpText(HTML("对于连续性变量，可以直接使用<br/>'Bar填充映射颜色1'与'Bar填充映射颜色2'<br/>作为<高值>与<低值>的颜色"))),
                                            fluidRow(
                                              column(6, colourInput("page5_vertical_barplot_fill_colour1", "Bar填充映射颜色1","#1F78B4")),
                                              column(6, colourInput("page5_vertical_barplot_fill_colour2", "Bar填充映射颜色2","#FFB300"))
                                            ),
                                            fluidRow(
                                              column(6, colourInput("page5_vertical_barplot_fill_colour3", "Bar填充映射颜色3","#FA2017")),
                                              column(6, colourInput("page5_vertical_barplot_fill_colour4", "Bar填充映射颜色4","#33A02C"))
                                            ),
                                          )
                                        ),
                                        bsCollapsePanel(
                                          title = "Bar label参数设置",
                                          value = "vertical_bar_label_settings",
                                          style = "warning",
                                          helpText("是否展示Bar Label"),
                                          fluidRow(
                                            column(6,selectInput("page5_vertical_barplot_show_bar_label","展示Bar Label",
                                                                 choices = c("展示" = TRUE,
                                                                             "不展示" = FALSE
                                                                 ))),
                                            column(6,conditionalPanel(condition = "input.page5_vertical_barplot_show_bar_label == 'TRUE'", 
                                                                      textInput("page5_vertical_barplot_bar_label", "Bar label所在列", value = "RichFactor")))
                                          ),
                                          conditionalPanel(condition = "input.page5_vertical_barplot_show_bar_label == 'TRUE'",
                                                           hr(),
                                                           helpText("bar label设置"),
                                                           fluidRow(
                                                             column(6, selectInput("page5_vertical_barplot_geom_text1_Font","bar label字体",
                                                                                   choices = c("普通" = "plain",
                                                                                               "加粗" = 'bold',
                                                                                               "斜体" = 'italic',
                                                                                               "加粗斜体" = 'bold.italic'),selected = c("加粗" = 'bold'))),
                                                             column(6,colourInput("page5_vertical_barplot_geom_text1_colour","bar label颜色","black"))
                                                           ),
                                                           textInput("page5_vertical_barplot_geom_text1_angle","bar label旋转角度",value = "0"),
                                                           sliderInput("page5_vertical_barplot_geom_text1_size","bar label字号",min = 1, max = 6, value = 3.5,step = 0.5),
                                                           sliderInput("page5_vertical_barplot_geom_text1_hjust","bar label水平移动",min = -1, max = 1, value = -0.5,step = 0.1),
                                                           sliderInput("page5_vertical_barplot_geom_text1_vjust","bar label垂直移动",min = -1, max = 1, value = 0,step = 0.1)
                                          )
                                        ),
                                        bsCollapsePanel(
                                          title = "Theme参数设置",
                                          value = "vertical_bar_Theme_settings",
                                          style = "warning",
                                          helpText("边框设置"),
                                          colourInput("page5_vertical_barplot_panel.border_colour","边框颜色","black"),
                                          sliderInput("page5_vertical_barplot_panel.border_linewidth","边框宽度",min = 0.1, max = 2, value = 1.5,step = 0.1),
                                          hr(),
                                          helpText("文本设置"),
                                          fluidRow(
                                            column(4, colourInput("page5_vertical_barplot_axis.text_colour","轴标签文本颜色","black")),
                                            column(4, selectInput("page5_vertical_barplot_axis.text_Font", "轴标签字体",
                                                                  choices = c("普通" = "plain",
                                                                              "加粗" = 'bold',
                                                                              "斜体" = 'italic',
                                                                              "加粗斜体" = 'bold.italic'
                                                                  ))),
                                            column(4, textInput("page5_vertical_barplot_axis.text_angle","x轴标签旋转角度",value = "45"))
                                          ),
                                          sliderInput("page5_vertical_barplot_axis.text_size","轴标签字号",min = 1, max = 20, value = 11),
                                          sliderInput("page5_vertical_barplot_axis.text_hjust","x轴标签水平移动",min = -1, max = 1, value = 1,step = 0.5),
                                          hr(),
                                          helpText("X,Y,legend标题设置"),
                                          fluidRow(
                                            column(6, colourInput("page5_vertical_barplot_axis.title_colour","X,Y,legend标题颜色","black")),
                                            column(6, selectInput("page5_vertical_barplot_axis.title_Font","X,Y,legend标题字体",
                                                                  choices = c("普通" = "plain",
                                                                              "加粗" = 'bold',
                                                                              "斜体" = 'italic',
                                                                              "加粗斜体" = 'bold.italic'
                                                                  ),selected = c("加粗" = 'bold')))
                                          ),
                                          sliderInput("page5_vertical_barplot_axis.title_size","X,Y,legend标签大小",min = 1, max = 20, value = 13),
                                          hr(),
                                          helpText("主标题设置"),
                                          fluidRow(
                                            column(4, colourInput("page5_vertical_barplot_plot.title_colour","主标题颜色","black")),
                                            column(4, selectInput("page5_vertical_barplot_plot.title_Font","主标题字体",
                                                                  choices = c("普通" = "plain",
                                                                              "加粗" = 'bold',
                                                                              "斜体" = 'italic',
                                                                              "加粗斜体" = 'bold.italic'
                                                                  ),selected = c("加粗" = 'bold')))
                                          ),
                                          sliderInput("page5_vertical_barplot_plot.title_hjust","主标题水平移动",min = -1, max = 1, value = 0,step = 0.1),
                                          sliderInput("page5_vertical_barplot_plot.title_size","主标题大小",min = 1, max = 20, value = 15),
                                          hr(),
                                          helpText("次标题设置"),
                                          fluidRow(
                                            column(4, colourInput("page5_vertical_barplot_plot.subtitle_colour","次标题颜色","black")),
                                            column(4, selectInput("page5_vertical_barplot_plot.subtitle_Font","次标题字体",
                                                                  choices = c("斜体" = 'italic',
                                                                              "加粗" = 'bold',
                                                                              "加粗斜体" = 'bold.italic',
                                                                              "普通" = "plain"
                                                                  ),
                                                                  selected = c("斜体" = 'italic')))
                                          ),
                                          sliderInput("page5_vertical_barplot_plot.subtitle_hjust","次标题水平移动",min = -1, max = 1, value = 0,step = 0.1),
                                          sliderInput("page5_vertical_barplot_plot.subtitle_size","次标题大小",min = 1, max = 20, value = 13),
                                          hr(),
                                          helpText("绘图边界设置"),
                                          sliderInput("page5_vertical_barplot_plot.margin_up","上边界",min = 0, max = 150, value = 20, step = 5),
                                          sliderInput("page5_vertical_barplot_plot.margin_down","下边界",min = 0, max = 150, value = 20, step = 5),
                                          sliderInput("page5_vertical_barplot_plot.margin_left","左边界",min = 0, max = 150, value = 100, step = 5),
                                          sliderInput("page5_vertical_barplot_plot.margin_right","右边界",min = 0, max = 150, value = 20, step = 5)
                                        ),
                                        bsCollapsePanel(
                                          title = "分面参数设置",
                                          value = "vertical_bar_facet_settings",
                                          style = "warning",
                                          helpText("设置"),
                                          selectInput("page5_vertical_barplot_facet","是否分面",
                                                      choices = c("分面" = TRUE,
                                                                  "不分面" = FALSE
                                                      ),selected = c("不分面" = FALSE)),
                                          conditionalPanel(
                                            condition = "input.page5_vertical_barplot_facet == 'TRUE'",
                                            textInput("page5_vertical_barplot_facet_by", "按哪一列分面", value = "ONTOLOGY"),
                                            helpText("分面标签背景颜色"),
                                            fluidRow(
                                              column(6,colourInput("page5_vertical_barplot_facet_fill_colour1","分面1背景颜色","#1F78B4")),
                                              column(6,colourInput("page5_vertical_barplot_facet_fill_colour2","分面2背景颜色","#FFB300"))
                                            ),
                                            fluidRow(
                                              column(6,colourInput("page5_vertical_barplot_facet_fill_colour3","分面3背景颜色","#FA2017")),
                                              column(6,colourInput("page5_vertical_barplot_facet_fill_colour4","分面4背景颜色","#33A02C"))
                                            ),
                                            helpText("分面标签边框颜色"),
                                            fluidRow(
                                              column(6,colourInput("page5_vertical_barplot_facet_border_colour1","分面1边框颜色","#1F78B4")),
                                              column(6,colourInput("page5_vertical_barplot_facet_border_colour2","分面2边框颜色","#FFB300"))
                                            ),
                                            fluidRow(
                                              column(6,colourInput("page5_vertical_barplot_facet_border_colour3","分面3边框颜色","#FA2017")),
                                              column(6,colourInput("page5_vertical_barplot_facet_border_colour4","分面4边框颜色","#33A02C"))
                                            ),
                                            helpText("字体字号"),
                                            fluidRow(
                                              column(12,selectInput("page5_vertical_barplot_facet_Font","分面字体",
                                                                    choices = c("普通" = "plain",
                                                                                "加粗" = 'bold',
                                                                                "斜体" = 'italic',
                                                                                "加粗斜体" = 'bold.italic'
                                                                    )))
                                            ),
                                            fluidRow(
                                              column(12, sliderInput("page5_vertical_barplot_facet_size", "分面字号",min = 5, max = 20, value = 10))
                                            )
                                          )  
                                        )
                                      )
                                    ),
                                    ########--------circlize_enrichplot面板--------########
                                    # conditionalPanel(
                                    #   condition = "input.page5_enrichplottype == 'page5_circlize_enrichplot'",
                                    #   ## 添加一个选择折叠框
                                    #   bsCollapse(
                                    #     id = "page5_circlize_enrichplot",
                                    #     open = "framework_settings",  # 默认关闭
                                    #     bsCollapsePanel(
                                    #       title = "绘图框架设置",
                                    #       value = "framework_settings",
                                    #       style = "warning",
                                    #       fluidRow(
                                    #         column(4, textInput("tgname_geneid", "选择geneID的列名", value = "geneid")),
                                    #         column(4, textInput("tgname_fdr", "选择FDR的列名", value = "pvalue")),
                                    #         column(4, textInput("tgname_logfc", "选择logFC的列名", value = "log2FoldChange"))
                                    #       )
                                    #     ),
                                    #     bsCollapsePanel(
                                    #       title = "元素参数设置",
                                    #       value = "element_settings",
                                    #       style = "warning",
                                    #       helpText("分别选择两组的绘图列名"),
                                    #       fluidRow(
                                    #         column(4, textInput("merged_name_geneid", "选择geneID的列名", value = "geneid")),
                                    #         column(4, textInput("merged_name_fdr1", "选择FDR_A的列名", value = "pvalue_1")),
                                    #         column(4, textInput("merged_name_logfc1", "选择logFC_A的列名", value = "log2FoldChange_1")),
                                    #         column(4, textInput("merged_name_fdr2", "选择FDR_B的列名", value = "pvalue_2")),
                                    #         column(4, textInput("merged_name_logfc2", "选择logFC_B的列名", value = "log2FoldChange_2"))
                                    #       )
                                    #     )
                                    #   )
                                    # ),
                                    ########--------cluster_enrichplot面板--------########
                                    conditionalPanel(
                                      condition = "input.page5_enrichplottype == 'page5_cluster_enrichplot'",
                                      ## 添加一个选择折叠框
                                      bsCollapse(
                                        id = "page5_cluster_enrichplot",
                                        open = "cluster_enrich_framework_settings",  # 默认关闭
                                        bsCollapsePanel(
                                          title = "绘图框架设置",
                                          value = "cluster_enrich_framework_settings",
                                          style = "warning",
                                          helpText("设置富集聚类图基础框架"),
                                          fluidRow(
                                            column(4, textInput("page5_cluster_enrich_colorBy", "以哪一列着色", value = "pvalue")),
                                            column(4, textInput("page5_cluster_enrich_nodeSize", "以哪一列映射点大小", value = "Count")),
                                            column(4, numericInput("page5_cluster_enrich_Description_warp", "换行最大字符数", value = 40))
                                          ),
                                          wellPanel(helpText(HTML("输入为GO富集时：着色推荐:pvalue 大小映射推荐:Count<br/>输入为GSEA时：着色推荐:NES 大小映射推荐:setSize"))),
                                          hr(),
                                          helpText("设置富集聚类图标题"),
                                          fluidRow(
                                            column(6, textInput("page5_cluster_enrich_title", "主标题", value = "GO Enrich Network")),
                                            column(6, textInput("page5_cluster_enrich_subtitle","次标题",value = "Enriched ONTOLOPGY of top 760 Upgrade genes"))
                                          )
                                        ),
                                        bsCollapsePanel(
                                          title = "参数设置",
                                          value = "cluster_enrich_colour_settings",
                                          style = "warning",
                                          helpText("分别选择两组的绘图列名"),
                                          fluidRow(
                                            column(4, selectInput("page5_cluster_enrich_simMethod","简化方法",
                                                                  choices = c("jaccard" = "jaccard",
                                                                              "cosine" = "cosine",
                                                                              "cor" = "cor"
                                                                  ))),
                                            column(4, selectInput("page5_cluster_enrich_clustMethod","聚类方法",
                                                                  choices = c("markov" = "markov",
                                                                              "hier" = "hier"
                                                                  ))),
                                            column(4,selectInput("page5_cluster_enrich_clustNameMethod","聚类群标签",
                                                                 choices = c("pagerank" = "pagerank",
                                                                             "hits" = "hits",
                                                                             "none" = "none"
                                                                 )))
                                          ),
                                          fluidRow(
                                            column(4, selectInput("page5_cluster_enrich_use_palette","配色方式",
                                                                  choices = c("使用配色版" = "TRUE",
                                                                              "手动配色" = "FALSE"
                                                                  ))),
                                            column(4, selectInput("page5_cluster_enrich_drawEllipses","添加类群外圈",
                                                                  choices = c("添加" = "TRUE",
                                                                              "不添加" = "FALSE"
                                                                  ))),
                                            column(4,selectInput("page5_cluster_enrich_repelLabels","避免标签重叠",
                                                                 choices = c("使用" = "TRUE",
                                                                             "不使用" = "FALSE"
                                                                 )))
                                          ),
                                          conditionalPanel(
                                            condition = "input.page5_cluster_enrich_use_palette == 'TRUE'",
                                            fluidRow(
                                              column(6, selectInput("page5_cluster_enrich_fill_palette","配色板",
                                                                    choices = c("蓝渐变" = "Blues",
                                                                                "绿渐变" = "Greens",
                                                                                "灰渐变" = "Greys",
                                                                                "橙渐变" = "Oranges",
                                                                                "紫渐变" = "Purples",
                                                                                "红渐变" = "Reds",
                                                                                "蓝绿渐变" = "BuGn",
                                                                                "绿紫渐变" = "BuPu",
                                                                                "绿蓝渐变" = "GnBu",
                                                                                "橙红渐变" = "OrRd",
                                                                                "紫蓝渐变" = "PuBu",
                                                                                "紫红渐变" = "PuRd",
                                                                                "红紫渐变" = "RdPu",
                                                                                "黄绿渐变" = "YlGn",
                                                                                "紫蓝绿渐变" = "PuBuGn",
                                                                                "黄绿蓝渐变" = "YlGnBu",
                                                                                "黄橙棕渐变" = "YlOrBr",
                                                                                "黄橙红渐变" = "YlOrRd",
                                                                                "棕绿离散" = "BrBG",
                                                                                "粉绿离散" = "PiYG",
                                                                                "紫绿离散" = "PRGn",
                                                                                "紫棕离散" = "PuOr",
                                                                                "红蓝离散" = "RdBu",
                                                                                "红灰离散" = "RdGy",
                                                                                "红黄蓝离散" = "RdYlBu",
                                                                                "红黄绿离散" = "RdYlGn",
                                                                                "光谱离散" = "Spectral"
                                                                    ))),
                                              column(6, selectInput("page5_cluster_enrich_fill_palette_direction", "颜色反向",
                                                                    choices = c("原向" = "TRUE",
                                                                                "反向" = "FALSE")
                                              ))
                                            )
                                          ),
                                          ## 分类变量手动配色
                                          conditionalPanel(
                                            condition = "input.page5_cluster_enrich_use_palette == 'FALSE'",
                                            fluidRow(
                                              column(4, colourInput("page5_cluster_enrich_fill_colour1", "Bar填充映射颜色1","#1F78B4")),
                                              column(4, colourInput("page5_cluster_enrich_fill_colour2", "Bar填充映射颜色2","#FFB300"))
                                            )
                                          ),
                                          
                                          sliderInput("page5_cluster_enrich_innerCutoff","同一聚类内节点之间的最低相似性阈值",min = 0, max = 1, value = 0.1, step = 0.01),
                                          sliderInput("page5_cluster_enrich_outerCutoff","不同聚类之间的相似性阈值",min = 0, max = 1, value = 0.5, step = 0.01),
                                          sliderInput("page5_cluster_enrich_minClusterSize","最小聚类通路数",min = 2, max = 15, value = 2),
                                          selectInput("page5_cluster_enrich_colorType","颜色映射方式",
                                                      choices = c("nes" = "nes",
                                                                  "pvalue" = "pval"
                                                      ),selected = c("pvalue" = "pval")),
                                          conditionalPanel(condition = "input.page5_cluster_enrich_colorType == 'pval'",
                                                           wellPanel(helpText(HTML("当使用'pval'方式映射颜色时<br/>设定 p 值对数转换的截断值<br/>以防止极端值影响颜色分布.")),
                                                                     sliderInput("page5_cluster_enrich_pCutoff","阈值的范围",min = -30, max = 0, value = -10, step = 1))
                                          ),
                                          sliderInput("page5_cluster_enrich_fontSize","字体大小",min = 1, max = 6, value = 3.5,step = 0.5)
                                        ),
                                        bsCollapsePanel(
                                          title = "Theme参数设置",
                                          value = "cluster_enrich_Theme_settings",
                                          style = "warning",
                                          helpText("legend设置"),
                                          fluidRow(
                                            column(6, colourInput("page5_cluster_enrich_legend.title_colour","legend标题颜色","black")),
                                            column(6, selectInput("page5_cluster_enrich_legend.title_Font","legend标题字体",
                                                                  choices = c("普通" = "plain",
                                                                              "加粗" = 'bold',
                                                                              "斜体" = 'italic',
                                                                              "加粗斜体" = 'bold.italic'
                                                                  ),selected = c("加粗" = 'bold')))
                                          ),
                                          sliderInput("page5_cluster_enrich_legend.title_size","legend标签大小",min = 1, max = 20, value = 13),
                                          fluidRow(
                                            column(6, colourInput("page5_cluster_enrich_legend.text_colour","legend文本颜色","black")),
                                            column(6, selectInput("page5_cluster_enrich_legend.text_Font","legend文本字体",
                                                                  choices = c("普通" = "plain",
                                                                              "加粗" = 'bold',
                                                                              "斜体" = 'italic',
                                                                              "加粗斜体" = 'bold.italic'
                                                                  ),selected = c("加粗" = 'bold')))
                                          ),
                                          sliderInput("page5_cluster_enrich_legend.text_size","legend文本大小",min = 1, max = 20, value = 11),
                                          hr(),
                                          helpText("主标题设置"),
                                          fluidRow(
                                            column(4, colourInput("page5_cluster_enrich_plot.title_colour","主标题颜色","black")),
                                            column(4, selectInput("page5_cluster_enrich_plot.title_Font","主标题字体",
                                                                  choices = c("普通" = "plain",
                                                                              "加粗" = 'bold',
                                                                              "斜体" = 'italic',
                                                                              "加粗斜体" = 'bold.italic'
                                                                  ),selected = c("加粗" = 'bold')))
                                          ),
                                          sliderInput("page5_cluster_enrich_plot.title_hjust","主标题水平移动",min = 0, max = 1, value = 0.5,step = 0.1),
                                          sliderInput("page5_cluster_enrich_plot.title_size","主标题大小",min = 1, max = 20, value = 15),
                                          hr(),
                                          helpText("次标题设置"),
                                          fluidRow(
                                            column(4, colourInput("page5_cluster_enrich_plot.subtitle_colour","次标题颜色","black")),
                                            column(4, selectInput("page5_cluster_enrich_plot.subtitle_Font","次标题字体",
                                                                  choices = c("斜体" = 'italic',
                                                                              "加粗" = 'bold',
                                                                              "加粗斜体" = 'bold.italic',
                                                                              "普通" = "plain"
                                                                  ),
                                                                  selected = c("斜体" = 'italic')))
                                          ),
                                          sliderInput("page5_cluster_enrich_plot.subtitle_hjust","次标题水平移动",min = 0, max = 1, value = 0.5,step = 0.1),
                                          sliderInput("page5_cluster_enrich_plot.subtitle_size","次标题大小",min = 1, max = 20, value = 13)
                                        )
                                      )
                                    ),
                                    
                                    # 通用控件
                                    wellPanel(
                                      sliderInput("page5_plot_panel_height", "展示区域高度", min = 100, max = 1080, value = 600),
                                      sliderInput("page5_plot_panel_width", "展示区域宽度", min = 100, max = 1080, value = 600),
                                      fluidRow(
                                        column(6, textInput("page5_plotwidth", "图片宽", value = 6)),
                                        column(6, textInput("page5_plotheight", "图片高", value = 6))
                                      ),
                                      radioButtons('page5_extPlot', '设置图片输出格式', 
                                                   choices = c( 'PDF' = 'pdf', "PNG" = 'png','JPEG' = 'jpeg'), 
                                                   inline = TRUE),
                                      downloadButton("page5_plotdownload", "下载图片",style = "color: white; background-color: #1abc9c; border-color: #16a085;")
                                    )
                                  ),
                                  cellWidths = c("70%", "30%")  # 调整为30%以更好显示控件
                                )
                              )
                    )
                  )
)

#######ID信息查询
page6 <- tabPanel("ID信息查询",icon = icon("magnifying-glass",style="color: #74C0FC;"),
                  sidebarLayout(
                    sidebarPanel(
                      width = 2,
                      position = "left",
                      # style = "width: 20vw;",
                      selectInput("page6species", "选择物种:",
                                  choices = c("拟南芥" = "athaliana",
                                              "水稻" = "osativa")),
                      radioButtons("page6geneIDInputMethod", "选择ID列表输入方式:",
                                   choices = c("上传文件" = "file", "粘贴列表" = "text"),
                                   selected = "file", inline = TRUE),
                      conditionalPanel(
                        condition = "input.page6geneIDInputMethod == 'file'",
                        fileInput("page6IDlist", "选择 geneID list 文件（仅一列）")
                      ),
                      conditionalPanel(
                        condition = "input.page6geneIDInputMethod == 'text'",
                        textAreaInput("page6IDText", "粘贴 geneID 列表（每行一个）", rows = 5,
                                      placeholder = "AT1G01010\nAT1G01020\n...\nor\nLOC_Os01g01010\nLOC_Os02g12340")
                      ),
                      useShinyjs(),
                      fluidRow(column(6,actionButton("page6searchstart", "开始查找",icon = icon("magnifying-glass"),style = "color: white; background-color: #3498db; border-color: #2980b9;")),
                               column(6,actionButton("page6_clear_btn", "清空输入", icon = icon("trash"),style = "color: white; background-color: #e74c3c; border-color: #c0392b;"))
                      ),
                      hr(),
                      div(downloadButton("page6tabledownloadData","下载表格",style = "color: white; background-color: #1abc9c; border-color: #16a085;"))
                    ),
                    mainPanel(
                      width = 10,
                      # style = "width: 70vw;padding-left: 0; margin-left: -100px;",
                      fluidPage(
                        splitLayout(div(helpText("结果展示"),
                                        dataTableOutput("page6table1"))
                                    # cellWidths = c("80%","60%")
                        )
                      )
                    )
                  )
)
######其他网页工具
page7 <- tabPanel("其他网页工具",icon = icon("link",style="color: #74C0FC;"),
                  fluidPage(
                    fluidRow(
                      column(6,
                             h3("其他绘图工具",style = "font-weight:bold;font-family: 'times'; font-size:14pt"),
                             p(em("韦恩图："), a("Jvenn", href="https://www.bioinformatics.com.cn/static/others/jvenn/example.html",style = "font-family: 'times'; font-size:12pt",target = "_blank"),style = "font-family: 'times'; font-size:12pt"),
                             p(em("进化树美化："), a("iTOL", href="https://itol.embl.de/",style = "font-family: 'times'; font-size:12pt",target = "_blank"),style = "font-family: 'times'; font-size:12pt"),
                             br(),
                             h3("基因组数据库",style = "font-weight:bold;font-family: 'times'; font-size:14pt"),
                             p(em("水稻："), a("RAP-DB", href="https://rapdb.dna.affrc.go.jp/",style = "font-family: 'times'; font-size:12pt",target = "_blank"),style = "font-family: 'times'; font-size:12pt"),
                             p(em("水稻："), a("RGAP", href="https://rice.uga.edu/",style = "font-family: 'times'; font-size:12pt",target = "_blank"),style = "font-family: 'times'; font-size:12pt"),
                             p(em("籼稻："), a("RIGW", href="http://rice.hzau.edu.cn/rice_rs3/",style = "font-family: 'times'; font-size:12pt",target = "_blank"),style = "font-family: 'times'; font-size:12pt"),
                             p(em("6000份水稻数据："), a("RiceAltas", href="http://60.30.67.242:18076/#/home",style = "font-family: 'times'; font-size:12pt",target = "_blank"),style = "font-family: 'times'; font-size:12pt"),
                             p(em("拟南芥："),a("TAIR", href="https://www.arabidopsis.org/",style = "font-family: 'times'; font-size:12pt",target = "_blank"), style = "font-family: 'times'; font-size:12pt"),
                             p(em("植物："),a("Ensembl Plants", href="http://plants.ensembl.org/index.html",style = "font-family: 'times'; font-size:12pt",target = "_blank"), style = "font-family: 'times'; font-size:12pt"),
                             p(em("动物："),a("Ensembl", href="https://www.ensembl.org/index.html",style = "font-family: 'times'; font-size:12pt",target = "_blank"), style = "font-family: 'times'; font-size:12pt"),
                             br(),
                      ),
                      column(6,
                             h3("蛋白相关",style = "font-weight:bold;font-family: 'times'; font-size:14pt"),
                             p(em("基因功能批量注释："), a("eggNog—mapper", href="http://eggnog-mapper.embl.de/",style = "font-family: 'times'; font-size:12pt",target = "_blank"),style = "font-family: 'times'; font-size:12pt"),
                             p(em("AlphaFold3："), a("AlphaFold3", href="https://alphafoldserver.com/",style = "font-family: 'times'; font-size:12pt",target = "_blank"),style = "font-family: 'times'; font-size:12pt"),
                             p(em("蛋白结合、结构、组装："), a("PDBePISA", href="https://www.ebi.ac.uk/pdbe/pisa/",style = "font-family: 'times'; font-size:12pt",target = "_blank"),style = "font-family: 'times'; font-size:12pt"),
                             p(em("蛋白互作预测："), a("STRING", href="https://cn.string-db.org/",style = "font-family: 'times'; font-size:12pt",target = "_blank"),style = "font-family: 'times'; font-size:12pt"),
                             p(em("蛋白ID信息"), a("UniProt", href="https://www.uniprot.org/",style = "font-family: 'times'; font-size:12pt",target = "_blank"),style = "font-family: 'times'; font-size:12pt"),
                             p(em("蛋白结构信息"), a("InterPro", href="https://www.ebi.ac.uk/interpro/",style = "font-family: 'times'; font-size:12pt",target = "_blank"),style = "font-family: 'times'; font-size:12pt"),
                             br(),
                             h3("转录因子相关",style = "font-weight:bold;font-family: 'times'; font-size:14pt"),
                             p(em("植物启动子分析："), a("PlantPAN 4.0", href="https://plantpan.itps.ncku.edu.tw/plantpan4/index.html",style = "font-family: 'times'; font-size:12pt",target = "_blank"),style = "font-family: 'times'; font-size:12pt"),
                             p(em("植物启动子分析："), a("PlantCare", href="https://bioinformatics.psb.ugent.be/webtools/plantcare/html/",style = "font-family: 'times'; font-size:12pt",target = "_blank"),style = "font-family: 'times'; font-size:12pt"),
                             p(em("水稻转录因子下游预测："), a("RiceTFtarget", href="https://cbi.njau.edu.cn/RiceTFtarget/",style = "font-family: 'times'; font-size:12pt",target = "_blank"),style = "font-family: 'times'; font-size:12pt"),
                             p(em("启动子结合预测："), a("JASPAR", href="https://jaspar.elixir.no/",style = "font-family: 'times'; font-size:12pt",target = "_blank"),style = "font-family: 'times'; font-size:12pt")
                             
                      )
                    )
                  )
)
###### just for fun~
page8 <- tabPanel("",
                  icon = icon("bilibili",class = "fa-brands fa-bilibili fa-bounce fa-xl",style = "color: #74C0FC;"),
                  useShinyjs(),
                  fluidRow(
                    column(12,
                           actionLink("textLink2", "🫵🤓你不准点开",
                                      style = "font-family: 'times'; font-size:30pt",
                                      # style = "color: green; text-decoration: underline; cursor: pointer;"
                           ),
                           br(), br(),
                           conditionalPanel(
                             condition = "output.showImage2",
                             div(
                               h3("你很不乖昂~赏你一点好东西~",style = "font-family: 'times'; font-size:30pt ;color:red ;font-weight:bold"
                               ),
                               uiOutput("displayImage2")  # 改为 uiOutput
                             )
                           )
                    )
                  )
)

###### qRT-PCR Calculator
page9 <- tabPanel("qRT-PCR计算器",
                  icon = icon("calculator",class = "fa-solid fa-calculator",style = "color: #74C0FC;"),
                  useShinyjs(),
                  # tabsetPanel(
                  #   ## 计算器panel
                  #   tabPanel("计算器",
                             sidebarLayout(
                               sidebarPanel(
                                 width = 2,
                                 h4("qRT-PCR计算器"),
                                 hr(),
                                 p("本计算器用于计算qRT-PCR实验中的相关参数。"),
                                 fileInput("page9_qrtpcr_file", "选择qRT-PCR结果文件"),
                                 checkboxInput('page9_header', '第一行作为表头', TRUE),
                                 radioButtons('page9_sep', '数据分隔符号', 
                                              c(Tab = '\t', Comma = ',', Semicolon = ';'), 
                                              selected = ',', inline = TRUE),
                                 fluidRow(
                                   column(6,textInput("page9_control_name", "对照组名称", value = "ZH11")),
                                   column(6,textInput("page9_reference_gene", "内参基因", value = "actin"))
                                 ),
                                 hr(),
                                 fluidRow(column(6,actionButton("page9_calculate_start", "开始计算",icon = icon("play"),style = "color: white; background-color: #3498db; border-color: #2980b9;")),
                                          column(6,actionButton("page9_clear_btn", "清空输入", icon = icon("trash"),style = "color: white; background-color: #e74c3c; border-color: #c0392b;"))
                                 )),
                               mainPanel(width = 10,
                                         fluidPage(actionLink("page9_showexampledata","点击查看计算器输入数据格式"),
                                                   bsCollapse(
                                                     open = "page9_collapse_output_data_all",
                                                     bsCollapsePanel(
                                                       title = "总计算结果",
                                                       value = "page9_collapse_output_data_all",
                                                       style = "danger",
                                                       div(
                                                         style = "overflow-y: auto;overflow-x: auto;white-space: nowrap;",
                                                         DT::DTOutput("page9_output_data_all"),
                                                         downloadButton("page9_download_data_all","下载计算结果",style = "color: white; background-color: #1abc9c; border-color: #16a085;")
                                                       )
                                                     )
                                                   ),
                                                   ## 主界面
                                                   div(
                                                     fluidRow(
                                                       column(6,
                                                              bsCollapse(
                                                                open = "page9_collapse_input_data",
                                                                bsCollapsePanel(
                                                                  title = "输入数据",
                                                                  value = "page9_collapse_input_data",
                                                                  style = "primary",
                                                                  div(
                                                                    style = "overflow-y: auto;overflow-x: auto;white-space: nowrap;",
                                                                    DT::DTOutput("page9_input_data")
                                                                  )
                                                                )
                                                              )
                                                       ),
                                                       column(6,
                                                              bsCollapse(
                                                                open = "page9_collapse_output_data_only_fc",
                                                                bsCollapsePanel(
                                                                  title = "可绘图数据",
                                                                  value = "page9_collapse_output_data_only_fc",
                                                                  style = "success",
                                                                  div(
                                                                    style = "overflow-y: auto;overflow-x: auto;white-space: nowrap;",
                                                                    DT::DTOutput("page9_output_data_only_fc"),
                                                                    downloadButton("page9_download_data_only_fc","下载绘图数据",style = "color: white; background-color: #1abc9c; border-color: #16a085;")
                                                                  )
                                                                )
                                                              )
                                                       )
                                                     )
                                                   )
                                         )
                               )
                             )
                  #   )
                  # )
)

# Define UI for application 
ui<-navbarPage(
  # title = div(
  #   tags$img(
  #     src = "https://raw.githubusercontent.com/EdmundFieldQIN/WgroupBioinfoToolBoxShinyApp/refs/heads/main/logo.jpg",
  #     # src = "logo.jpg",
  #     style = "
  #   height: auto;
  #   max-height: 56px;
  #   width: auto;
  #   # max-width: 200px;
  #   margin-top: -20px;
  #   margin-left:-15px;
  # "
  #   )),
  title = tags$div(
    tags$head(tags$style(HTML("
      .navbar-brand {
        padding-top: 0px !important;
        padding-bottom: 0px !important;
      }

      .navbar-brand img {
        height: 51px !important;  /* 使图片高度与navbar一致 */
        margin-left: -15px !important; /* 贴紧左边 */
        margin-top: 0px !important;
      }
    "))),
    tags$img(
      src = "https://raw.githubusercontent.com/EdmundFieldQIN/WgroupBioinfoToolBoxShinyApp/refs/heads/main/logo.jpg"
      # src = "https://gitee.com/EdmundFieldQIN/WgroupBioinfoToolBoxShinyApp/raw/main/logo.jpg"
    )
  ),
  
  # 主题
  theme = shinytheme("cosmo"),
  windowTitle = "WgroupBioInfoToolBox",
  homepage,
  navbarMenu("差异分析",
             icon = icon("arrow-right-arrow-left",class = "fa-solid fa-arrow-right-arrow-left",style = "color: #74C0FC;"),
             page1,
             "----",
             page3
  ),
  page2,
  navbarMenu("富集分析" ,icon = icon("arrows-to-dot",class = "fa-thin fa-arrows-to-dot",style = "color: #74C0FC;"),
             page4,
             "----",
             page5),
  page9,
  page6,
  page7,
  page8
)
# Define server logic 
server <- function(input,output,session) {
  
  ###########################模块TPM&heatmap响应################################
  ###矩阵数据处理、cor计算、TPM计算
  mat<-eventReactive(input$TPplotStart,{
    if (input$TP=="heatmap"){
      rawmat<-input$matrixFile
      if (is.null(rawmat))
        return(NULL)
      htmat <- read.delim(file = rawmat$datapath, header = input$headerT, row.names = 1,as.is = TRUE, sep = input$sepT,encoding='UTF-8')
      htmat
    }else if(input$TP=="cor"){
      rawmat<-input$matrixFile
      if (is.null(rawmat))
        return(NULL)
      countdata <- read.delim(file = rawmat$datapath, header = input$headerT, row.names = 1,as.is = TRUE, sep = input$sepT,encoding='UTF-8')
      #countdata<-as.matrix(countdata)
      countdata<-as.matrix(countdata)
      #cors<-cor(countdata, method = c("pearson"))
      
      if (input$matNum=="one"){
        cors<-Hmisc::rcorr(countdata,type = input$cor_m)
        cors$P[is.na(cors$P)]<-1
        cors
      }else{
        rawmat2<-input$secondMatFile
        if (is.null(rawmat2))
          return(NULL)
        countdata2 <- read.delim(file = rawmat2$datapath, header = input$headerT, row.names = 1,as.is = TRUE, sep = input$sepT,encoding='UTF-8')
        countdata2<-as.matrix(countdata2)
        cors<-Hmisc::rcorr(countdata,countdata2, type = input$cor_m)
        cors$P[is.na(cors$P)]<-1
        cors
      }
      
    }
    
  })
  
  ###计算TPM CPM FPKM
  calculatemat<-eventReactive(input$TPcalculateStart,{
    rawmat<-input$matrixFile
    if (is.null(rawmat))
      return(NULL)
    countdata <- read.delim(file = rawmat$datapath, header = input$headerT, row.names = 1,as.is = TRUE, sep = input$sepT,encoding='UTF-8')
    metadata <- countdata[,1:5]#提取基因信息count数据前的几列
    countdata <- countdata[,6:ncol(countdata)]#提取counts数，counts数据主题部分
    cpm <- t(t(countdata)/colSums(countdata) * 1000000)#参考cpm定义
    #avg_cpm <- data.frame(avg_cpm=rowMeans(cpm))
    #-----TPM Calculation------
    kb <- metadata[,5] / 1000
    rpk <- countdata / kb
    tpm <- t(t(rpk)/colSums(rpk) * 1000000)
    fpkm <- t(t(rpk)/colSums(countdata) * 10^6)
    if (input$CP=="TPM")
      return(tpm)
    if (input$CP=="CPM")
      return(cpm)
    if (input$CP=="FPKM")
      return(fpkm)
  })
  
  
  #列分组信息输入
  colgroup<-eventReactive(input$TPplotStart,{
    colg<-input$colgroupFile
    if (is.null(colg))
      return(NULL)
    cg <- read.delim(file = colg$datapath, header = T, row.names = 1,as.is = TRUE, sep = "\t",encoding='UTF-8')
    cg
  })
  
  
  #行分组信息输入
  rowgroup<-eventReactive(input$TPplotStart,{
    colg<-input$rowgroupFile
    if (is.null(colg))
      return(NULL)
    cg <- read.delim(file = colg$datapath, header = T, row.names = 1,as.is = TRUE, sep = "\t",encoding='UTF-8')
    cg
  })
  
  output$table_tpm <- renderTable({
    
    if (input$plotOrcal=="plot"){
      if (is.null(mat()))
        return(NULL)
      head(mat())
    }else{
      if (is.null(calculatemat()))
        return(NULL)
      head(calculatemat())
    }
  },rownames = T)
  
  
  drawht<-reactive({
    ###热图绘制
    scale_test <- apply(mat(), 2, function(x){log2(x+1)})
    
    pheatmap(mat =scale_test,
             cluster_cols = input$row_c,
             cluster_rows = input$col_c,
             angle_col = "45",
             cellwidth=15,
             colorRampPalette(colors = c(input$col_tp2,input$col_tp3,input$col_tp1))(100),
             annotation_col = colgroup(),
             annotation_row = rowgroup(),
             annotation_names_col = input$col_a,
             annotation_names_row = input$row_a
             # annotation_names_row = F
    )#适用于平均值
  })
  
  
  drawcor<-reactive({
    corrplot::corrplot(mat()$r, type = input$corType, method =input$corMethod,order = input$corOrder,tl.col = "black", tl.srt = 45,
                       col = colorRampPalette(colors = c(input$col_tp2,input$col_tp3,input$col_tp1))(100),
                       is.corr = input$corlim,p.mat = mat()$P, insig = "label_sig",
                       sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = input$sig_col)
    
  })
  
  
  output$plot_ht<-renderPlot({
    if (input$plotOrcal=="calculate")
      return(NULL)
    
    if (input$TP=="cor"){
      if (is.null(drawcor()))
        return(NULL)
      drawcor()
    }else if(input$TP=="heatmap"){ 
      if (is.null(drawht()))
        return(NULL)
      drawht()}
  })
  
  
  #下载图片
  output$download_ht<-downloadHandler(
    filename = function() {
      if (input$TP=="cor"){
        paste('corHeatmap', Sys.Date(), '.',input$ext2ht, sep='')
      }else if (input$TP=="heatmap"){
        paste('Heatmap', Sys.Date(), '.',input$ext2ht, sep='')
      }
      
    },
    content=function(file){
      if (input$TP=="cor"){
        if (input$ext2ht=="png"){
          png(file)
        }else if(input$ext2ht=="pdf"){
          pdf(file,width =as.numeric(input$htwidth), height = as.numeric(input$htheight) )
        }else{
          jpeg(file)
        }
        corrplot::corrplot(mat()$r, type = input$corType, method =input$corMethod,order = input$corOrder,tl.col = "black", tl.srt = 45,
                           col = colorRampPalette(colors = c(input$col_tp2,input$col_tp3,input$col_tp1))(100),
                           is.corr = input$corlim,p.mat = mat()$P, insig = "label_sig",
                           sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = input$sig_col)
        dev.off()
        #ggsave(file,plot =replayPlot(p1()), width = as.numeric(input$htwidth), height = as.numeric(input$htheight))
      }else if(input$TP=="heatmap"){
        ggsave(file,plot = drawht(), width = as.numeric(input$htwidth), height = as.numeric(input$htheight))
      }
      
    })
  
  
  ##下载tpm
  output$download_tpm<-downloadHandler(filename = function() {paste("TPM", Sys.Date(), ".csv", sep="")},content=function(file) {
    write.csv(calculatemat(), file)
  })
  ################################差异表达分析##################################
  #DESeq2流程
  #eventReactive 隔离流程
  result<-eventReactive(input$action,{
    if (input$tools=="DESeq2"){
      ### DESeq2流程
      rawmat <- input$matFile
      if (is.null(rawmat))
        return(NULL)
      
      mycounts <- read.delim(file = rawmat$datapath, 
                             header = input$header, 
                             row.names = 1,
                             as.is = TRUE, 
                             sep = input$sep,
                             encoding = 'UTF-8')
      
      rawfile <- input$conditionFile
      if (is.null(rawfile))
        return(NULL)
      
      conditiondata <- read.table(file = rawfile$datapath,
                                  header = F,
                                  encoding = 'UTF-8')
      
      condition <- c(conditiondata[,1])
      condition <- factor(condition)
      colData <- data.frame(row.names = colnames(mycounts), condition)
      
      dds <- DESeqDataSetFromMatrix(mycounts, colData, design = ~condition)
      dds <- DESeq(dds)
      res <- results(dds, contrast = c("condition", input$case, input$control))
      res <- res[order(res$pvalue),]
      res <- na.omit(res)
      
      # 获取标准化计数
      normalized_counts <- counts(dds, normalized = TRUE)
      
      # 计算每个组的baseMean
      case_samples <- which(colData$condition == input$case)
      control_samples <- which(colData$condition == input$control)
      
      case_baseMean <- rowMeans(normalized_counts[, case_samples, drop = FALSE])
      control_baseMean <- rowMeans(normalized_counts[, control_samples, drop = FALSE])
      
      # 转换为数据框并添加基因ID列和分组baseMean
      res_df <- as.data.frame(res) %>% 
        rownames_to_column(var = "gene_id")
      
      # 添加各组的baseMean（只取res_df中存在的基因）
      res_df[[paste0(input$case, "_baseMean")]] <- case_baseMean[res_df$gene_id]
      res_df[[paste0(input$control, "_baseMean")]] <- control_baseMean[res_df$gene_id]
      
      # 重新排列列的顺序
      res_df <- res_df[, c("gene_id", 
                           paste0(input$control, "_baseMean"), 
                           paste0(input$case, "_baseMean"),
                           "baseMean","log2FoldChange", "lfcSE", "stat", "pvalue", "padj")]
      
      # 筛选上调和下调基因
      up <- subset(res_df, padj < 0.05 & log2FoldChange > 1)
      down <- subset(res_df, padj < 0.05 & log2FoldChange < -1)
      
      # 根据用户选择返回相应数据集
      if (input$dataset == "ALL") {
        datasetOutput <- res_df
      } else if (input$dataset == "Up") {
        datasetOutput <- up
      } else {
        datasetOutput <- down
      }
      
      return(datasetOutput)
  } else {
      ### edgeR流程
      rawmat<-input$matFile
      if (is.null(rawmat))
        return(NULL)
      x<-read.delim(file = rawmat$datapath, header=input$header,row.names=1, stringsAsFactors=FALSE, as.is = TRUE, sep = input$sep,encoding='UTF-8')
      rawfile<-input$conditionFile
      if (is.null(rawfile))
        return(NULL)
      group<-read.table(file = rawfile$datapath,header = F,encoding='UTF-8')
      
      y <- DGEList(counts=x, group=group[,1])
      keep <- rowSums(cpm(y)>1) >= 2
      y <- y[keep,]
      y$samples$lib.size <- colSums(y$counts)
      y <- calcNormFactors(y)
      
      group<-factor(group[,1])
      design<-model.matrix(~0+group)
      colnames(design)<-levels(group)
      rownames(design)<-colnames(x)
      
      y <- estimateGLMCommonDisp(y,design)
      y <- estimateGLMTrendedDisp(y,design)
      y <- estimateGLMTagwiseDisp(y,design)
      fit<-glmFit(y,design)
      
      design_df<-as.data.frame(design)
      control_num<-which(names(design_df)==input$control)
      case_num<-which(names(design_df)==input$case)
      if (control_num <case_num){
        case_num=-1
        
      }
      if (control_num >case_num){
        control_num=-1
      }
      lrt<-glmLRT(fit,contrast = c(case_num,control_num))
      
      
      # Diff table
      diff <- as.data.frame(topTags(lrt, n = nrow(y))) %>%
        rownames_to_column(var = "gene_id")
      # Summary table
      diff<-na.omit(diff)
      up<-subset(diff, FDR < 0.05 & logFC > 1)
      #uplist<-rownames(up)
      down<-subset(diff, FDR < 0.05 & logFC < -1)
      #downlist<-rownames(down)
      if (input$dataset=="ALL"){
        datasetOutput<-diff
      }
      else if (input$dataset=="Up"){
        datasetOutput<-up
      }
      else {
        datasetOutput<-down
      }
      datasetOutput
    }
  })
  
  
  
  output$Result <- renderDataTable({
    
    if (is.null(result()))
      return(NULL)
    result()
  })
  
  
  
  output$downlodData<-downloadHandler(filename = function() {paste(input$dataset, input$tools, ".txt", sep="")},content=function(file) {
    write.table(result(), file,row.names = F,col.names = T,sep = "\t",quote = F)
  })
  
  ################################ FUNC 3 Volacno ##############################
  source(system.file("extdata/volcano_plot_enhanced_function.R", package = "org.Osativa.eg.db"))
  # 准备画图df
  volcanodf <- eventReactive(input$plotstart, {
    if (input$volcanoplottype == 'vol_norm' | input$volcanoplottype == 'vol_enhance'){
      if (input$inputMethod == "file") {
        rawdf <- input$volcanoFile
        if (is.null(rawdf)) return(NULL)
        dataset <- read.delim(file = rawdf$datapath, header = input$header2, stringsAsFactors = FALSE, as.is = TRUE, sep = input$sep2, encoding = 'UTF-8')
      } else {
        rawText <- input$volcanoText
        if (is.null(rawText) || rawText == "") return(NULL)
        dataset <- read.delim(text = rawText, header = input$header2, stringsAsFactors = FALSE, as.is = TRUE, sep = input$sep2, encoding = 'UTF-8')
      }
      
      cut_off_fdr <- as.numeric(input$fdrvalue)
      cut_off_logFC <- as.numeric(input$FCvalue)
      
      idpos <- which(names(dataset) == input$name1)
      fdrpos <- which(names(dataset) == input$name2)
      fcpos <- which(names(dataset) == input$name3)
      
      dataset$change <- ifelse(dataset[, fdrpos] < cut_off_fdr & abs(dataset[, fcpos]) >= cut_off_logFC,
                               ifelse(dataset[, fcpos] > cut_off_logFC, 'Up', 'Down'),
                               'NotSig')
      dataset <- na.omit(dataset)
      dataset <- dataset[, c(idpos, fdrpos, fcpos, ncol(dataset))]
      dataset
    }else if(input$volcanoplottype == 'vol_twogroup'){
      # 使用 rename() 指定旧列名换为新列名
      twogroup_data <- as.data.frame(twogroup_data())
      names(twogroup_data)[names(twogroup_data) == input$merged_name_geneid] <- "geneid"
      names(twogroup_data)[names(twogroup_data) == input$merged_name_fdr1] <- "pvalue_1"
      names(twogroup_data)[names(twogroup_data) == input$merged_name_logfc1] <- "log2FoldChange_1"
      names(twogroup_data)[names(twogroup_data) == input$merged_name_fdr2] <- "pvalue_2"
      names(twogroup_data)[names(twogroup_data) == input$merged_name_logfc2] <- "log2FoldChange_2"
      # 赋值阈值
      tgpvalue_th <- as.numeric(input$fdrvalue)
      tgfc_th <- as.numeric(input$FCvalue)
      
      twogroup_data <- na.omit(twogroup_data)
      
      twogroup_data[which(twogroup_data$pvalue_1 <= tgpvalue_th & twogroup_data$pvalue_2 <= tgpvalue_th),'type1'] <- 'Sig'
      twogroup_data[which(twogroup_data$pvalue_1 > tgpvalue_th | twogroup_data$pvalue_2 > tgpvalue_th),'type1'] <- 'No'
      #标记差异倍数（默认 |log2FC| >= log2(1.5)）
      twogroup_data[which(twogroup_data$log2FoldChange_1 <= -tgfc_th & twogroup_data$log2FoldChange_2 <= -tgfc_th),'type2'] <- 'ADBD'
      twogroup_data[which(twogroup_data$log2FoldChange_1 >= tgfc_th & twogroup_data$log2FoldChange_2 <= -tgfc_th),'type2'] <- 'AUBD'
      twogroup_data[which(twogroup_data$log2FoldChange_1 <= -tgfc_th & twogroup_data$log2FoldChange_2 >= tgfc_th),'type2'] <- 'ADBU'
      twogroup_data[which(twogroup_data$log2FoldChange_1 >= tgfc_th & twogroup_data$log2FoldChange_2 >= tgfc_th),'type2'] <- 'AUBU'
      twogroup_data[which(twogroup_data$log2FoldChange_1 <= tgfc_th & twogroup_data$log2FoldChange_1 >= -tgfc_th & twogroup_data$log2FoldChange_2 <= tgfc_th & twogroup_data$log2FoldChange_2 >= -tgfc_th) ,'type2'] <- 'Norm'
      twogroup_data[is.na(twogroup_data$type2),'type2'] <- 'No'
      #标记配色类型
      twogroup_data[which(twogroup_data$type1 == 'Sig' & twogroup_data$type2 == 'ADBD') , 'Type'] <- "ADBD"
      twogroup_data[which(twogroup_data$type1 == 'Sig' & twogroup_data$type2 == 'AUBD') , 'Type'] <- "AUBD"
      twogroup_data[which(twogroup_data$type1 == 'Sig' & twogroup_data$type2 == 'ADBU') , 'Type'] <- "ADBU"
      twogroup_data[which(twogroup_data$type1 == 'Sig' & twogroup_data$type2 == 'AUBU') , 'Type'] <- "AUBU"
      twogroup_data[which(twogroup_data$type2 == 'Norm'), 'Type'] <- "Not Diff"
      twogroup_data[is.na(twogroup_data$Type), 'Type'] <- 'No Sig'
      #点大小
      twogroup_data[,"Size"] <- abs(twogroup_data[,"log2FoldChange_1"])+abs(twogroup_data[,"log2FoldChange_2"])
      
      twogroup_data
    }
  })
  # 准备添加geneiddf
  iddf <- eventReactive(input$plotstart, {
    if (input$geneIDInputMethod == "file") {
      rawdf <- input$geneIDlist
      if (is.null(rawdf)) return(NULL)
      IDdata <- read.delim(file = rawdf$datapath, header = FALSE, stringsAsFactors = FALSE, as.is = TRUE, sep = "\t", encoding = 'UTF-8')
    } else {
      text_data <- input$geneIDText
      if (is.null(text_data) || text_data == "") return(NULL)
      IDdata <- data.frame(V1 = unlist(strsplit(text_data, "\n")), stringsAsFactors = FALSE)
    }
    
    colnames(IDdata) <- colnames(volcanodf())[1]  # 设置列名与主数据的gene列一致
    merged <- merge(IDdata, volcanodf(), by.x = 1, by.y = 1, all.x = TRUE)
    if(input$volcanoplottype == 'vol_twogroup'){
      merged <- merged %>%
        mutate(
          nudge_x_dir = ifelse(log2FoldChange_1 > 0, 3, -3),  # 右正左负
          nudge_y_dir = ifelse(log2FoldChange_2 > 0, 3, -3)   # 上正下负
        )
      merged
    }else{
      merged
    }
  })
  
  #######
  # 九象限火山图：合并两组数据
  tgmergedata <- eventReactive(input$datamergestart, {
    req(input$tgvolcanoFile1, input$tgvolcanoFile2)
    
    tgdata1 <- read.table(file = input$tgvolcanoFile1$datapath,
                          header = input$tgheader, 
                          sep = input$tgsep,
                          stringsAsFactors = FALSE,
                          encoding = 'UTF-8')
    
    tgdata2 <- read.table(file = input$tgvolcanoFile2$datapath,
                          header = input$tgheader, 
                          sep = input$tgsep,
                          stringsAsFactors = FALSE,
                          encoding = 'UTF-8')
    
    # 提取需要的列
    tgdata1 <- tgdata1[, c(input$tgname_geneid, input$tgname_logfc, input$tgname_fdr)]
    tgdata2 <- tgdata2[, c(input$tgname_geneid, input$tgname_logfc, input$tgname_fdr)]
    
    # 合并数据并添加后缀
    tgdata <- full_join(tgdata1, tgdata2, by = input$tgname_geneid, suffix = c("_1", "_2")) %>% 
      drop_na()
    
    tgdata
  })
  
  # 九象限火山图：上传已合并的数据
  tguploadeddata <- reactive({
    req(input$volcanoFile_merged)
    if (is.null(input$volcanoFile_merged)) return(NULL)
    read.table(file = input$volcanoFile_merged$datapath,
               header = input$tgheader, 
               sep = input$tgsep,
               stringsAsFactors = FALSE,
               encoding = 'UTF-8')
  })
  
  # 决定使用哪种数据（合并的或直接上传的）
  twogroup_data <- reactive({
    if (!is.null(input$volcanoFile_merged)) {
      tguploadeddata()
    } else if (!is.null(tgmergedata())) {
      tgmergedata()
    } else {
      NULL
    }
  })
  
  # 下载合并数据
  output$downloadmergedData <- downloadHandler(
    filename = function() {
      paste0("volcano_merged_data_", Sys.Date(), ".txt")
    },
    content = function(file) {
      write.table(tgmergedata(), file, sep = "\t", row.names = FALSE, quote = FALSE,col.names = T)
    }
  )
  
  
  output$text1 <- renderDataTable({
    if (input$volcanoplottype == "vol_twogroup") {
      # 显示两组合并数据或直接上传的合并数据
      req(twogroup_data())
      twogroup_data()
    } else {
      # 原有单组数据显示逻辑
      if (input$inputMethod == "file") {
        raw <- input$volcanoFile
        if (is.null(raw)) return(NULL)
        df <- read.delim(file = raw$datapath, 
                         header = input$header2, 
                         stringsAsFactors = FALSE, 
                         sep = input$sep2, 
                         encoding = 'UTF-8')
      } else {
        rawText <- input$volcanoText
        if (is.null(rawText) || rawText == "") return(NULL)
        df <- read.delim(text = rawText, 
                         header = input$header2, 
                         stringsAsFactors = FALSE, 
                         sep = input$sep2, 
                         encoding = 'UTF-8')
      }
      return(df)
    }
  }, options = list(pageLength = 5))
  # 绘图
  plot1_obj <- reactive({
    if (is.null(volcanodf()))
      return(NULL)
    ## 绘制vol_norm
    if (input$volcanoplottype == 'vol_norm') {
      base_plot <- ggplot(volcanodf(), aes(x = volcanodf()[,input$name3], y = -log10(volcanodf()[,input$name2]), colour = change)) +
        geom_point(alpha = input$alpha_norm, size = input$size_norm) +
        coord_cartesian(xlim = c(-max(abs(volcanodf()[[input$name3]])), max(abs(volcanodf()[[input$name3]])))) +                    # 设置x和y轴范围
        scale_color_manual(values = c(input$col_down, input$col_nonsig, input$col_up)) +
        geom_vline(xintercept = c(-as.numeric(input$FCvalue), as.numeric(input$FCvalue)), lty = 4, col = "grey20", lwd = 0.8) +
        geom_hline(yintercept = -log10(as.numeric(input$fdrvalue)), lty = 4, col = "grey20", lwd = 0.8) +
        labs(x = "Log2(FoldChange)", y = "-Log10(FDR)", title = input$title_norm) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5), 
              legend.position = "right", 
              legend.title = element_blank(),
              panel.grid = element_blank())
      
      if (!is.null(iddf())) {
        base_plot <- base_plot + 
          geom_text_repel(data = iddf(),
                          aes(x = iddf()[, input$name3], y = -log10(iddf()[,input$name2]), label = iddf()[,input$name1]),
                          colour = input$textcol_norm,
                          size = input$textsize_norm,
                          force = 20, box.padding = 1, point.padding = 1, hjust = 0.5,
                          min.segment.length = 0,
                          arrow = arrow(length = unit(0.01, "npc"), type = "open", ends = "last"),
                          segment.color = "grey20", segment.size = 0.5, segment.alpha = 0.8, nudge_y = 1)
      }
      
      return(base_plot)
      ## 绘制vol_enhance  
    } else if (input$volcanoplottype == 'vol_enhance') {
      base_plot <- volcano_plot_enhanced(volcanodf(),
                                         num_symbol = as.numeric(input$veshowgenenum),
                                         logFC=input$name3,
                                         FDR=input$name2,
                                         logFC_Value=as.numeric(input$FCvalue),
                                         FDR_Value=as.numeric(input$fdrvalue),
                                         Symbol=input$name1,
                                         y_increased=as.numeric(input$vey_increased),
                                         labs=input$velabel_name,
                                         labs_decreased=as.numeric(input$velabs_decreased),
                                         plot_title=input$vetitle,
                                         up_color=input$veupcol,
                                         down_color=input$vedowncol) +
        scale_color_gradientn(colours = c(input$vecol1, input$vecol2, input$vecol3, input$vecol4, input$vecol5),
                              values = seq(0, 1, 0.2)) +
        scale_fill_gradientn(colours = c(input$vecol1, input$vecol2, input$vecol3, input$vecol4, input$vecol5),
                             values = seq(0, 1, 0.2))
      
      if (!is.null(iddf())) {
        base_plot <- volcano_plot_enhanced(volcanodf(),
                                           num_symbol = 0,
                                           logFC=input$name3,
                                           FDR=input$name2,
                                           logFC_Value=as.numeric(input$FCvalue),
                                           FDR_Value=as.numeric(input$fdrvalue),
                                           Symbol=input$name1,
                                           y_increased=as.numeric(input$vey_increased),
                                           labs=input$velabel_name,
                                           labs_decreased=as.numeric(input$velabs_decreased),
                                           plot_title=input$vetitle,
                                           up_color=input$veupcol,
                                           down_color=input$vedowncol) +
          scale_color_gradientn(colours = c(input$vecol1, input$vecol2, input$vecol3, input$vecol4, input$vecol5),
                                values = seq(0, 1, 0.2)) +
          scale_fill_gradientn(colours = c(input$vecol1, input$vecol2, input$vecol3, input$vecol4, input$vecol5),
                               values = seq(0, 1, 0.2)) +
          geom_text_repel(data = iddf(),
                          aes(x = iddf()[, input$name3], y = -log10(iddf()[, input$name2]), label = iddf()[, input$name1]),
                          nudge_x = 0.5, nudge_y = 0.2,
                          segment.curvature = -0.1, segment.ncp = 3,
                          direction = "y", hjust = "left",
                          max.overlaps = 200)
      }
      
      return(base_plot)
    } else if (input$volcanoplottype == 'vol_twogroup'){
      
      
      #排序，为了使作图时显著的点绘制在前方（减少被遮盖）
      twogroup_df <- volcanodf()
      twogroup_df$Type <- factor(twogroup_df$Type,
                                 levels = c("ADBU", "AUBU", "ADBD", "AUBD", "Not Diff", "No Sig"),
                                 labels = c(input$tgtext1, input$tgtext2, input$tgtext3, 
                                            input$tgtext4, input$tgtext5, input$tgtext6))
      twogroup_df <- twogroup_df[order(twogroup_df$Type, decreasing = TRUE), ]
      
      
      # 计算计数 - 使用原始分类
      ADBD_count <- sum(twogroup_df$type2 == "ADBD" & twogroup_df$type1 == "Sig")
      AUBD_count <- sum(twogroup_df$type2 == "AUBD" & twogroup_df$type1 == "Sig")
      ADBU_count <- sum(twogroup_df$type2 == "ADBU" & twogroup_df$type1 == "Sig")
      AUBU_count <- sum(twogroup_df$type2 == "AUBU" & twogroup_df$type1 == "Sig")
      NoSig_count <- sum(twogroup_df$type2 == "Norm" | twogroup_df$type1 == "No")
      limit <- max(c(max(abs(twogroup_df$log2FoldChange_1), na.rm = TRUE),max(abs(twogroup_df$log2FoldChange_2), na.rm = TRUE)))
      
      tg_textsize <- as.numeric(input$tgtextsize)
      # 绘制
      base_plot <- ggplot(twogroup_df, aes(log2FoldChange_1, log2FoldChange_2)) +
        geom_point(aes(color = Type), alpha = as.numeric(input$alpha_twogroup), show.legend = input$tgshowlegend,size = as.numeric(input$size_twogroup)) +
        scale_size(range = c(0, 4)) +
        scale_color_manual(limits = c(input$tgtext1,input$tgtext2,input$tgtext3, input$tgtext4, input$tgtext5, input$tgtext6), 
                           values = c(input$tgcol1, input$tgcol2, input$tgcol3, input$tgcol4, input$tgcol5, input$tgcol6)) +
        labs(
          title = input$tgtitle, # 标题
          x = input$tgxlab,                               # x轴标签
          y = input$tgylab,                               # y轴标签
          color = "Type"                          # 图例标题
        ) +
        geom_vline(xintercept = c(-as.numeric(input$FCvalue),as.numeric(input$FCvalue)), linetype = "dashed", color = "grey40", linewidth = 1) +  # x轴阈值线
        geom_hline(yintercept = c(-as.numeric(input$FCvalue),as.numeric(input$FCvalue)), linetype = "dashed", color = "grey40",linewidth = 1) +  # y轴阈值线
        coord_cartesian(xlim = c(-limit, limit), 
                        ylim = c(-limit, limit)) +                    # 设置x和y轴范围
        theme_bw(base_size = tg_textsize)  + # 添加左下角注释
        theme(
          panel.background = element_rect(color = 'black', fill = 'transparent'),
          # panel.background = element_blank(),         # 设置背景为空白
          panel.grid.major = element_blank(),         # 移除主网格线
          panel.grid.minor = element_blank(),         # 移除次网格线
          panel.border = element_rect(colour = "black", fill = NA, size = 2), # 加粗外框
          axis.line = element_line(color = "black"),  # 添加坐标轴线
          axis.ticks = element_line(color = "black",linewidth = 2),  # 设置坐标轴刻度线颜色
          plot.title = element_text(hjust = 0.5, size = tg_textsize+3, face = "bold"),
          # axis.title = element_text(size = 16),
          axis.text = element_text(size = tg_textsize),
          legend.title = element_text(size = tg_textsize),
          legend.text = element_text(size = tg_textsize),
          legend.key = element_blank(),
          legend.key.size = unit(10,"points"),
          axis.title.x = element_text(color = "black", size = tg_textsize, face = "bold"), # x轴标题设置
          axis.title.y = element_text(color = "black", size = tg_textsize, face = "bold")  # y轴标题设置
        )
      
      if (input$tgshownumber){
        base_plot <- base_plot +                              # 使用简约主题
          annotate("text", x = limit-1, y = -limit, label = paste("n:",AUBD_count), size = 6, color = input$tgcol4,fontface = "bold") +
          annotate("text", x = -limit+1, y = -limit, label = paste("n:",ADBD_count), size = 6, color = input$tgcol3,fontface = "bold") + # 添加右上角注释
          annotate("text", x = limit-1, y = limit, label = paste("n:",AUBU_count), size = 6, color = input$tgcol2,fontface = "bold", ) + # 添加左下角注释
          annotate("text", x = -limit+1, y = limit, label = paste("n:",ADBU_count), size = 6, color = input$tgcol1,fontface = "bold"#, family = "Arial"
          )
      }
      
      if (!is.null(iddf())) {
        iddf <- as.data.frame(iddf())
        base_plot <- base_plot + 
          geom_text_repel(data = iddf,
                          aes(log2FoldChange_1, log2FoldChange_2,label = iddf[, 1],color = Type),
                          show.legend = F,
                          size = 4,
                          box.padding = 3, point.padding = 1, hjust = 0.5,
                          min.segment.length = 0,
                          arrow = arrow(length = unit(0.01, "npc"), type = "open", ends = "last"),
                          segment.color = "grey20", segment.size = 1, segment.alpha = 0.8,
                          force = 20,           # 增大排斥力（默认1）
                          force_pull = 0.5,     # 减小吸引力（默认1）
                          nudge_x = iddf$nudge_x_dir,  # 动态偏移
                          nudge_y = iddf$nudge_y_dir,segment.ncp = 3,
                          direction = "x",max.overlaps =100)
      }
      return(base_plot)
      
    }
    
    return(NULL)
  })
  
  
  # 图片输出框
  output$plot1 <- renderPlot({
    if (is.null(plot1_obj())) return(NULL)
    plot1_obj()
  },
  width = function() input$volcano_plot_panel_width,
  height = function() input$volcano_plot_panel_height,
  res = 96)
  # 图片下载框
  output$plot1downloadData <- downloadHandler(
    filename = function() {
      paste('volcanoPlot', Sys.Date(), '.', input$extPlot, sep = '')
    },
    content = function(file) {
      ggsave(filename = file, plot = plot1_obj(),
             width = as.numeric(input$plot1width),
             height = as.numeric(input$plot1height), dpi = 300)
    }
  )
  ######################### FUNC 4 clusterprofiler #############################
  # 获取当前物种的注释数据库 - 使用 reactive 包装
  annotation_data <- reactive({
    if(input$species == "other"){
      req(input$Gene2Term2Name)
      raw_terminfo <- input$Gene2Term2Name
      Gene2Term2Name <- read.delim(
        file = raw_terminfo$datapath, 
        header = TRUE, 
        stringsAsFactors = FALSE, 
        sep = "\t",
        encoding = 'UTF-8'
      )
      list(
        term2gene = Gene2Term2Name[, c(2, 1)],
        term2name = Gene2Term2Name[, c(2, 3)]
      )
    } else {
      list(
        orgdb = switch(input$species,
                       "athaliana" = org.At.tair.db::org.At.tair.db,
                       "oryza" = org.Osativa.eg.db::org.Osativa.eg.db),
        kegg_db = switch(input$species,
                         "athaliana" = "ath",
                         "oryza" = "osa")
      )
    }
  })
  
  # 加载水稻KEGG数据
  rice_kegg_data <- reactive({
    if(input$species == "oryza") {
      list(
        pathway2gene = read.xlsx(system.file("extdata/pathway2gene.xlsx", package = "org.Osativa.eg.db")),
        pathway2name = read.xlsx(system.file("extdata/pathway2name.xlsx", package = "org.Osativa.eg.db"))
      )
    } else {
      NULL
    }
  })
  
  # 执行富集分析
  enrichment_result <- eventReactive(input$run_enrich_analysis, {
    req(input$enrich_gene_list)
    genes <- unlist(strsplit(input$enrich_gene_list, "\n"))
    genes <- trimws(genes[genes != ""])
    
    if (length(genes) == 0) return(NULL)
    
    # 获取当前物种的注释数据
    annot_data <- annotation_data()
    rice_kegg <- rice_kegg_data()
    
    if (input$analysis_type == "go") {
      if (input$species == "other") {
        req(annot_data$term2gene, annot_data$term2name)
        enricher(
          gene = genes,
          TERM2GENE = annot_data$term2gene,
          TERM2NAME = annot_data$term2name,
          # ont = input$go_ontology,
          pvalueCutoff = input$pvalue_cutoff,
          qvalueCutoff = input$qvalue_cutoff
        )
      } else {
        req(annot_data$orgdb)
        keyType <- ifelse(input$species == "athaliana", "TAIR", "GID")
        enrichGO(
          gene = genes,
          OrgDb = annot_data$orgdb,
          keyType = keyType,
          ont = input$go_ontology,
          pvalueCutoff = input$pvalue_cutoff,
          qvalueCutoff = input$qvalue_cutoff
        )
      }
    } else {  # KEGG分析
      if (input$species == "other") {
        req(annot_data$term2gene, annot_data$term2name)
        enricher(
          gene = genes,
          TERM2GENE = annot_data$term2gene,
          TERM2NAME = annot_data$term2name,
          pvalueCutoff = input$pvalue_cutoff,
          qvalueCutoff = input$qvalue_cutoff
        )
      } else if (input$species == "athaliana") {
        enrichKEGG(
          gene = genes,
          organism = annot_data$kegg_db,
          keyType = "kegg",
          pvalueCutoff = input$pvalue_cutoff,
          qvalueCutoff = input$qvalue_cutoff
        )
      } else {  # 水稻
        req(rice_kegg$pathway2gene, rice_kegg$pathway2name)
        enricher(
          gene = genes,
          TERM2GENE = rice_kegg$pathway2gene,
          TERM2NAME = rice_kegg$pathway2name,
          pvalueCutoff = input$pvalue_cutoff,
          qvalueCutoff = input$qvalue_cutoff
        )
      }
    }
  })
  
  # 显示富集结果表格
  output$enrichment_table <- renderDT({
    ##############################R4.4以上时使用################################
    # result <- enrichment_result()@result
    # if (is.null(result)) return(NULL)
    # 
    # datatable(as.data.frame(result) %>%
    #             dplyr::filter(pvalue <= input$pvalue_cutoff) %>%
    #             dplyr::filter(qvalue <= input$qvalue_cutoff),
    #           options = list(pageLength = 10, scrollX = TRUE),
    #           rownames = FALSE)
    
    ############################# R4.2.1时使用##################################
    # 假设enrich_result是旧版clusterProfiler的富集结果对象
    result <- enrichment_result()@result
    if (is.null(result)) return(NULL)
    
    if (input$go_ontology == "ALL"){
      datatable(as.data.frame(result) %>%
                  tidyr::separate(col = GeneRatio, sep = "/", into = c("n1", "n2"), remove = F) %>%
                  tidyr::separate(col = BgRatio, sep = "/", into = c("n3", "n4"), remove = F) %>%
                  dplyr::mutate(RichFactor = as.numeric(as.numeric(n1)/as.numeric(n3))) %>%
                  dplyr::mutate(FoldEnrichment = as.numeric((as.numeric(n1)*as.numeric(n4))/(as.numeric(n2)*as.numeric(n3)))) %>% 
                  dplyr::filter(pvalue <= input$pvalue_cutoff) %>%
                  dplyr::filter(qvalue <= input$qvalue_cutoff) %>%
                  dplyr::select(ONTOLOGY,ID,Description,GeneRatio,BgRatio,RichFactor,FoldEnrichment,pvalue,p.adjust,qvalue,geneID,Count),
                options = list(pageLength = 10, scrollX = TRUE),rownames = FALSE
      )
    } else {
      datatable(as.data.frame(result) %>%
                  tidyr::separate(col = GeneRatio, sep = "/", into = c("n1", "n2"), remove = F) %>%
                  tidyr::separate(col = BgRatio, sep = "/", into = c("n3", "n4"), remove = F) %>%
                  dplyr::mutate(RichFactor = as.numeric(as.numeric(n1)/as.numeric(n3))) %>%
                  dplyr::mutate(FoldEnrichment = as.numeric((as.numeric(n1)*as.numeric(n4))/(as.numeric(n2)*as.numeric(n3)))) %>% 
                  dplyr::filter(pvalue <= input$pvalue_cutoff) %>%
                  dplyr::filter(qvalue <= input$qvalue_cutoff) %>%
                  dplyr::select(ID,Description,GeneRatio,BgRatio,RichFactor,FoldEnrichment,pvalue,p.adjust,qvalue,geneID,Count),
                options = list(pageLength = 10, scrollX = TRUE),rownames = FALSE
      )
    }
  })
  ##############################################################################
  
  
  # 绘制富集结果图
  enrichplot_obj <- reactive({
    result <- enrichment_result()
    if (is.null(result)) return(NULL)

    
    
    
    
    # result <- enrichment_result()@result
    # if (is.null(result)) return(NULL)
    # 
    # if (input$go_ontology == "ALL"){
    #   result1 <- as.data.frame(enrichment_result()@result) %>%
    #               tidyr::separate(col = GeneRatio, sep = "/", into = c("n1", "n2"), remove = F) %>%
    #               tidyr::separate(col = BgRatio, sep = "/", into = c("n3", "n4"), remove = F) %>%
    #               dplyr::mutate(RichFactor = as.numeric(as.numeric(n1)/as.numeric(n3))) %>%
    #               dplyr::mutate(FoldEnrichment = as.numeric((as.numeric(n1)*as.numeric(n4))/(as.numeric(n2)*as.numeric(n3)))) %>% 
    #               dplyr::filter(pvalue <= input$pvalue_cutoff) %>%
    #               dplyr::filter(qvalue <= input$qvalue_cutoff) %>%
    #               dplyr::select(ONTOLOGY,ID,Description,GeneRatio,BgRatio,RichFactor,FoldEnrichment,pvalue,p.adjust,qvalue,geneID,Count)
    #   return(result1)
    # } else {
    #   result1 <- as.data.frame(enrichment_result()@result) %>%
    #               tidyr::separate(col = GeneRatio, sep = "/", into = c("n1", "n2"), remove = F) %>%
    #               tidyr::separate(col = BgRatio, sep = "/", into = c("n3", "n4"), remove = F) %>%
    #               dplyr::mutate(RichFactor = as.numeric(as.numeric(n1)/as.numeric(n3))) %>%
    #               dplyr::mutate(FoldEnrichment = as.numeric((as.numeric(n1)*as.numeric(n4))/(as.numeric(n2)*as.numeric(n3)))) %>% 
    #               dplyr::filter(pvalue <= input$pvalue_cutoff) %>%
    #               dplyr::filter(qvalue <= input$qvalue_cutoff) %>%
    #               dplyr::select(ID,Description,GeneRatio,BgRatio,RichFactor,FoldEnrichment,pvalue,p.adjust,qvalue,geneID,Count)
    #   return(result1)
    # }
    
    
    
    
    
    
    
    
    
    
    
    
    
    show_cat <- min(input$show_category, nrow(as.data.frame(result)))
    # 根据选择的绘图类型创建不同的图
    plot_obj <- switch(input$enrich_plot_type,
                       "barplot" = {
                         barplot(result, showCategory = show_cat, 
                                 font.size = input$enrichplotfontsize)
                       },
                       "dotplot" = {
                         enrichplot::dotplot(result, showCategory = show_cat, 
                                 font.size = input$enrichplotfontsize)
                       },
                       "goplot" = {
                         if (input$analysis_type != "go") {
                           return(ggplot() + 
                                    geom_text(aes(x = 0.5, y = 0.5, 
                                                  label = "GO关系图仅适用于GO分析"), 
                                              size = 6) +
                                    theme_void())
                         } else if (input$go_ontology == "ALL"){
                           return(ggplot() +
                                    geom_text(aes(x = 0.5, y = 0.5,
                                                  label = "GO关系图仅适用于GO类别非全部(ALL)时分析"),
                                              size = 6) +
                                    theme_void())
                         }
                         tryCatch({
                           # 使用topGO的goplot函数
                           goplot(result, showCategory = show_cat)
                         }, error = function(e) {
                           ggplot() + 
                             geom_text(aes(x = 0.5, y = 0.5, 
                                           label = paste("无法绘制GO关系图:", e$message)), 
                                       size = 6) +
                             theme_void()
                         })
                       },
                       "emapplot" = {
                         tryCatch({
                           emapplot(pairwise_termsim(result), 
                                    showCategory = show_cat,
                                    shadowtext = TRUE,
                                    repel = FALSE # 解决标签重叠问题
                                    )
                         }, error = function(e) {
                           ggplot() + 
                             geom_text(aes(x = 0.5, y = 0.5, 
                                           label = paste("无法绘制emapplot:", e$message)), 
                                       size = 6) +
                             theme_void()
                         })
                       },
                       "cnetplot" = {
                         tryCatch({
                           cnetplot(result, showCategory = show_cat,
                                    colorEdge = T,node_label = "gene",
                                    circular = input$cnetplotcricos)
                         }, error = function(e) {
                           ggplot() + 
                             geom_text(aes(x = 0.5, y = 0.5, 
                                           label = paste("无法绘制cnetplot:", e$message)), 
                                       size = 6) +
                             theme_void()
                         })
                       },
                       # 默认使用dotplot
                       enrichplot::dotplot(result, showCategory = show_cat, 
                               font.size = input$enrichplotfontsize)
    )
    
    # 添加标题（如果不是ggplot对象则不添加）
    if (inherits(plot_obj, "ggplot")) {
      plot_obj <- plot_obj + 
        ggtitle(input$enrichplottitle) +
        theme(plot.title = element_text(hjust = 0.5, size = 16))
    }
    
    return(plot_obj)
  })
  
  # 渲染图形
  output$enrichment_plot <- renderPlot({
    if (is.null(enrichplot_obj())) {
      ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, 
                      label = "无显著富集结果或输入基因无效"), 
                  size = 6) +
        theme_void()
    } else {
      enrichplot_obj()
    }
  },
  width = function() input$enrich_plot_panel_width,
  height = function() input$enrich_plot_panel_height,
  res = 96)
  
  # 下载结果
  output$download_enrich_results <- downloadHandler(
    filename = function() {
      paste(input$analysis_type, "_enrichment_results_", Sys.Date(), ".txt",sep = "")
    },
    content = function(file) {
      write.table(
        # as.data.frame(enrichment_result())
        if (input$go_ontology == "ALL"){
          as.data.frame(enrichment_result()@result) %>%
            tidyr::separate(col = GeneRatio, sep = "/", into = c("n1", "n2"), remove = F) %>%
            tidyr::separate(col = BgRatio, sep = "/", into = c("n3", "n4"), remove = F) %>%
            dplyr::mutate(RichFactor = as.numeric(as.numeric(n1)/as.numeric(n3))) %>%
            dplyr::mutate(FoldEnrichment = as.numeric((as.numeric(n1)*as.numeric(n4))/(as.numeric(n2)*as.numeric(n3)))) %>% 
            dplyr::select(ONTOLOGY,ID,Description,GeneRatio,BgRatio,RichFactor,FoldEnrichment,pvalue,p.adjust,qvalue,geneID,Count)
        } else {
          as.data.frame(enrichment_result()@result) %>%
            tidyr::separate(col = GeneRatio, sep = "/", into = c("n1", "n2"), remove = F) %>%
            tidyr::separate(col = BgRatio, sep = "/", into = c("n3", "n4"), remove = F) %>%
            dplyr::mutate(RichFactor = as.numeric(as.numeric(n1)/as.numeric(n3))) %>%
            dplyr::mutate(FoldEnrichment = as.numeric((as.numeric(n1)*as.numeric(n4))/(as.numeric(n2)*as.numeric(n3)))) %>% 
            dplyr::select(ID,Description,GeneRatio,BgRatio,RichFactor,FoldEnrichment,pvalue,p.adjust,qvalue,geneID,Count)
        }
        , file, row.names = FALSE,quote = F,sep = "\t",col.names = T)
    }
  )
  
  # 下载图片
  output$enrichplotdownload <- downloadHandler(
    filename = function() {
      paste(input$analysis_type, "_", input$enrich_plot_type, "_", Sys.Date(), ".", input$extenrichPlot,sep = "")
    },
    content = function(file) {
      # 设置图片尺寸
      width <- as.numeric(input$enrichplotwidth)
      height <- as.numeric(input$enrichplotheight)
      
      # 根据选择的格式保存
      if (input$extenrichPlot == "pdf") {
        ggsave(file, plot = enrichplot_obj(), width = width, height = height, device = "pdf")
      } else if (input$extenrichPlot == "png") {
        ggsave(file, plot = enrichplot_obj(), width = width, height = height, device = "png", dpi = 300)
      } else {
        ggsave(file, plot = enrichplot_obj(), width = width, height = height, device = "jpeg", dpi = 300)
      }
    }
  )
  ###设置示例数据
  genelisttest<-c(paste("AT1G0",c(101:105),"0",sep = ""))
  term2genetest<-cbind(c(paste("GO:0000",c(411:415),"0",sep = "")),genelisttest)
  term2nametest<-cbind(c(paste("GO:0000",c(411:413),"0",sep = "")),c("tRNA binding","exocyst","Golgi membrane"))
  
  
  ################################### FUN 5 ####################################
  page5_plot_data <- eventReactive(input$page5_plotstart,{
    page5_raw <- input$page5_EnrichFile
    if (is.null(page5_raw)) return(NULL)
    page5_raw_plot_data <- as.data.frame(read.delim(file = page5_raw$datapath, 
                                                    header = input$page5_header, 
                                                    stringsAsFactors = FALSE, 
                                                    sep = input$page5_sep, 
                                                    encoding = 'UTF-8'))
    
    
    ## 气泡图
    if (input$page5_enrichplottype == 'page5_bubbleplot') {
      
      # 把GeneRatio和BgRatio转换为数值形式
      data1 <- page5_raw_plot_data %>%
        tidyr::separate(col = GeneRatio, sep = "/", into = c("n1", "n2"), remove = F) %>%
        dplyr::mutate(GeneRatio = as.numeric(n1)/as.numeric(n2)) %>%
        tidyr::separate(col = BgRatio, sep = "/", into = c("n3", "n4"), remove = F) %>%
        dplyr::mutate(BgRatio = as.numeric(n3)/as.numeric(n4))
      # 如果不排序就按照Description原生顺序排列  
      if (input$page5_bubbleplot_data_order) {
        data1 <- data1 %>% 
          dplyr::arrange(desc(if (input$page5_bubbleplot_data_order_by %in% c('pvalue', 'qvalue','p.adjust')) {
            -log10(.data[[input$page5_bubbleplot_data_order_by]])
          } else {
            .data[[input$page5_bubbleplot_data_order_by]]
          }
          )) %>%
          clusterProfiler::mutate(Description = str_wrap(Description, width = input$page5_bubbleplot_Description_warp)) %>%
          dplyr::mutate(Description = factor(Description, levels = rev(Description), ordered = T))
      } else {
        data1 <- data1 %>% 
          clusterProfiler::mutate(Description = str_wrap(Description, width = input$page5_bubbleplot_Description_warp)) %>%
          dplyr::mutate(Description = factor(Description, levels = unique(Description)))
      }
      
      return(data1)
      
      
      ## 横向Barplot
    } else if (input$page5_enrichplottype == 'page5_horizontal_barplot') {
      
      # 把GeneRatio和BgRatio转换为数值形式
      data1 <- page5_raw_plot_data %>%
        tidyr::separate(col = GeneRatio, sep = "/", into = c("n1", "n2"), remove = F) %>%
        dplyr::mutate(GeneRatio = as.numeric(n1)/as.numeric(n2)) %>%
        tidyr::separate(col = BgRatio, sep = "/", into = c("n3", "n4"), remove = F) %>%
        dplyr::mutate(BgRatio = as.numeric(n3)/as.numeric(n4))
      # 如果不排序就按照Description原生顺序排列  
      if (input$page5_horizontal_barplot_data_order) {
        data1 <- data1 %>% 
          dplyr::arrange(desc(if (input$page5_horizontal_barplot_data_order_by %in% c('pvalue', 'qvalue','p.adjust')) {
            -log10(.data[[input$page5_horizontal_barplot_data_order_by]])
          } else {
            .data[[input$page5_horizontal_barplot_data_order_by]]
          }
          )) %>%
          clusterProfiler::mutate(Description = str_wrap(Description, width = input$page5_horizontal_barplot_Description_warp)) %>%
          dplyr::mutate(Description = factor(Description, levels = rev(Description), ordered = T))
      } else {
        data1 <- data1 %>% 
          clusterProfiler::mutate(Description = str_wrap(Description, width = input$page5_horizontal_barplot_Description_warp)) %>%
          dplyr::mutate(Description = factor(Description, levels = unique(Description)))
      }
      
      
      ## 纵向Barplot
    } else if (input$page5_enrichplottype == 'page5_vertical_barplot') {
      # 把GeneRatio和BgRatio转换为数值形式
      data1 <- page5_raw_plot_data %>%
        tidyr::separate(col = GeneRatio, sep = "/", into = c("n1", "n2"), remove = F) %>%
        dplyr::mutate(GeneRatio = as.numeric(n1)/as.numeric(n2)) %>%
        tidyr::separate(col = BgRatio, sep = "/", into = c("n3", "n4"), remove = F) %>%
        dplyr::mutate(BgRatio = as.numeric(n3)/as.numeric(n4))
      # 如果不排序就按照Description原生顺序排列  
      if (input$page5_vertical_barplot_data_order) {
        data1 <- data1 %>% 
          dplyr::arrange(if (input$page5_vertical_barplot_data_order_by %in% c('pvalue', 'qvalue','p.adjust')) {
            -log10(.data[[input$page5_vertical_barplot_data_order_by]])
          } else {
            .data[[input$page5_vertical_barplot_data_order_by]]
          }
          ) %>%
          clusterProfiler::mutate(Description = str_wrap(Description, width = input$page5_vertical_barplot_Description_warp)) %>%
          dplyr::mutate(Description = factor(Description, levels = rev(Description), ordered = T))
      } else {
        data1 <- data1 %>% 
          clusterProfiler::mutate(Description = str_wrap(Description, width = input$page5_vertical_barplot_Description_warp)) %>%
          dplyr::mutate(Description = factor(Description, levels = unique(Description)))
      }
      return(data1)
      
      
      
      ## 富集聚类图
    } else if (input$page5_enrichplottype == 'page5_cluster_enrichplot') {
      
      return(page5_raw_plot_data)
      
      
      ## 环形富集图
    } else if (input$page5_enrichplottype == 'page5_circlize_enrichplot') {
      return(NULL)
    } else {
      return(NULL)
    }
  })
  
  
  
  ########--------绘图--------#########
  page5_plot_obj <- reactive({
    page5_plot_data_df <- page5_plot_data()
    
    ########--------气泡图--------########
    if (input$page5_enrichplottype == 'page5_bubbleplot') {
      p1 <- ggplot(data = page5_plot_data_df) + 
        geom_point(
          aes(x = .data[[input$page5_bubbleplot_x]], 
              y = .data[[input$page5_bubbleplot_y]], 
              size = .data[[input$page5_bubbleplot_size]], 
              # 判断如果是pvalue或者qvalue则取log
              fill = if (input$page5_bubbleplot_fill %in% c('pvalue', 'qvalue','p.adjust')) {
                -log10(.data[[input$page5_bubbleplot_fill]])
              } else {
                .data[[input$page5_bubbleplot_fill]]
              }
              # fill = -log10(.data[[input$page5_bubbleplot_fill]])
          ), 
          shape = input$page5_bubbleplot_bubble_shape, 
          alpha = input$page5_bubbleplot_bubble_alpha,
          stroke = input$page5_bubbleplot_bubble_stroke,
          colour = input$page5_bubbleplot_bubble_colour,
          show.legend = as.logical(input$page5_bubbleplot_show_legend)
        ) + 
        scale_fill_gradient(
          low = input$page5_bubbleplot_fill_low_colour,
          high = input$page5_bubbleplot_fill_height_colour,
          name = if (input$page5_bubbleplot_fill %in% c('pvalue', 'qvalue','p.adjust')) {
            paste0("-log10(", input$page5_bubbleplot_fill, ")")
          } else {
            input$page5_bubbleplot_fill
          }
        ) +
        scale_size(range = c(3,8),
                   guide = guide_legend(override.aes = list(fill = input$page5_bubbleplot_legend_bubble_colour))) +
        labs(title = input$page5_bubbleplot_title,subtitle = input$page5_bubbleplot_subtitle) + 
        theme_bw() + 
        theme(
          axis.text = element_text(color = input$page5_bubbleplot_axis.text_colour, size = input$page5_bubbleplot_axis.text_size,face = input$page5_bubbleplot_axis.text_Font),
          axis.title.x = element_text(color = input$page5_bubbleplot_axis.title_colour, size = input$page5_bubbleplot_axis.title_size,face = input$page5_bubbleplot_axis.title_Font),
          axis.title.y = element_text(color = input$page5_bubbleplot_axis.title_colour, size = input$page5_bubbleplot_axis.title_size,face = input$page5_bubbleplot_axis.title_Font),
          legend.title = element_text(color = input$page5_bubbleplot_axis.title_colour, size = input$page5_bubbleplot_axis.title_size,face = input$page5_bubbleplot_axis.title_Font),
          plot.title = element_text(color = input$page5_bubbleplot_plot.title_colour, size = input$page5_bubbleplot_plot.title_size,face = input$page5_bubbleplot_plot.title_Font),
          plot.subtitle = element_text(color = input$page5_bubbleplot_plot.subtitle_colour, size = input$page5_bubbleplot_plot.subtitle_size,face = input$page5_bubbleplot_plot.subtitle_Font),
          strip.text = element_text(size = input$page5_bubbleplot_facet_size,face = input$page5_bubbleplot_facet_Font,family = 'sans'),
          panel.background = if (input$page5_bubbleplot_panel.background) {element_blank()} else {NULL},         # 设置背景为空白
          panel.grid.major = if (!input$page5_bubbleplot_panel.grid.major) {element_blank()} else {NULL},         # 移除主网格线
          panel.grid.minor = if (!input$page5_bubbleplot_panel.grid.minor) {element_blank()} else {NULL},         # 移除次网格线
          panel.border = element_rect(colour = input$page5_bubbleplot_panel.border_colour, fill = NA, size = input$page5_bubbleplot_panel.border_size), # 加粗外
        )
      
      if (input$page5_bubbleplot_facet) {
        p2 <- p1 + 
          facet_grid2(rows = vars(.data[[input$page5_bubbleplot_facet_by]]) ,scales = "free_y",space = "free_y",
                      strip = strip_themed(background_y = elem_list_rect(fill = c(input$page5_bubbleplot_facet_colour1,input$page5_bubbleplot_facet_colour2,input$page5_bubbleplot_facet_colour3)))
          )
        return(p2)
      } else {
        return(p1)
      }
      
      ########--------横向Barplot--------########
    } else if (input$page5_enrichplottype == 'page5_horizontal_barplot') {
      
      # 预先geom_bar计算X轴数据
      if (input$page5_horizontal_barplot_x %in% c('pvalue', 'qvalue','p.adjust')) {
        page5_plot_data_df$x_value <- -log10(page5_plot_data_df[[input$page5_horizontal_barplot_x]])
        x_lab <- paste0("-log10(", input$page5_horizontal_barplot_x, ")")
      } else {
        page5_plot_data_df$x_value <- page5_plot_data_df[[input$page5_horizontal_barplot_x]]
        x_lab <- input$page5_horizontal_barplot_x
      }
      
      # 预先计算geom_bar填充颜色数据
      if (input$page5_horizontal_barplot_fill %in% c('pvalue', 'qvalue','p.adjust')) {
        page5_plot_data_df$fill_value <- -log10(page5_plot_data_df[[input$page5_horizontal_barplot_fill]])
        fill_lab <- paste0("-log10(", input$page5_horizontal_barplot_fill, ")")
      } else {
        page5_plot_data_df$fill_value <- page5_plot_data_df[[input$page5_horizontal_barplot_fill]]
        fill_lab <- input$page5_horizontal_barplot_fill
      }
      
      # 检测geom_bar的fill变量类型(数值型且唯一变量在4种以上)
      is_fill_continuous <- is.numeric(page5_plot_data_df$fill_value) && 
        length(unique(page5_plot_data_df$fill_value[!is.na(page5_plot_data_df$fill_value)])) > 4
      
      # 预先计算geneid_color
      if (input$page5_horizontal_barplot_geom_text2_colour_by %in% c('pvalue', 'qvalue','p.adjust')) {
        page5_plot_data_df$geneid_color <- -log10(page5_plot_data_df[[input$page5_horizontal_barplot_geom_text2_colour_by]])
        geneid_lab <- paste0("-log10(", input$page5_horizontal_barplot_geom_text2_colour_by, ")")
      } else {
        page5_plot_data_df$geneid_color <- page5_plot_data_df[[input$page5_horizontal_barplot_geom_text2_colour_by]]
        geneid_lab <- input$page5_horizontal_barplot_geom_text2_colour_by
      }
      
      # 绘图
      {if (!as.logical(input$page5_horizontal_barplot_use_fill)) {
        # 情况1：不使用fill映射，直接指定一个颜色
        p <- ggplot() +
          geom_bar(data = page5_plot_data_df,
                   aes(x = x_value,
                       y = .data[[input$page5_horizontal_barplot_y]]),
                   fill = input$page5_horizontal_barplot_fill_single_colour,  # 单一颜色
                   width = input$page5_horizontal_barplot_bar_width,
                   alpha = input$page5_horizontal_barplot_bar_alpha,
                   colour = input$page5_horizontal_barplot_bar_colour,
                   linewidth = input$page5_horizontal_barplot_bar_linewidth,
                   just = input$page5_horizontal_barplot_bar_just,
                   show.legend = FALSE,  # 单色不需要图例
                   stat = 'identity')
      } else {
        p <- ggplot() +
          geom_bar(data = page5_plot_data_df,
                   aes(
                     x = x_value,
                     y = .data[[input$page5_horizontal_barplot_y]],
                     fill = fill_value
                   ),
                   width = input$page5_horizontal_barplot_bar_width,
                   alpha = input$page5_horizontal_barplot_bar_alpha,
                   colour = input$page5_horizontal_barplot_bar_colour,
                   linewidth = input$page5_horizontal_barplot_bar_linewidth,
                   just = input$page5_horizontal_barplot_bar_just,
                   show.legend = as.logical(input$page5_horizontal_barplot_show_legend),
                   stat = 'identity') +
          # 填充颜色设置
          {if (as.logical(input$page5_horizontal_barplot_barfill_use_palette) && is_fill_continuous) {
            scale_fill_distiller(name = fill_lab,
                                 palette = input$page5_horizontal_barplot_barfill_palette, 
                                 direction = as.numeric(input$page5_horizontal_barplot_barfill_palette_direction))
          } else if (!as.logical(input$page5_horizontal_barplot_barfill_use_palette) && is_fill_continuous) {
            scale_fill_gradient(name = fill_lab,
                                low = input$page5_horizontal_barplot_fill_colour2,
                                high = input$page5_horizontal_barplot_fill_colour1)
          } else {
            scale_fill_manual(name = fill_lab,
                              values = c(input$page5_horizontal_barplot_fill_colour1,
                                         input$page5_horizontal_barplot_fill_colour2,
                                         input$page5_horizontal_barplot_fill_colour3,
                                         input$page5_horizontal_barplot_fill_colour4))
          }}
      }}
      p <- p +
        # 设置x轴与y轴间隔为0
        scale_x_continuous(expand = c(0,0),
                           limits = c(0, max(if (input$page5_horizontal_barplot_x %in% c('pvalue', 'qvalue','p.adjust')) {
                             -log10(page5_plot_data_df[[input$page5_horizontal_barplot_x]])
                           } else {
                             page5_plot_data_df[[input$page5_horizontal_barplot_x]]
                           }, na.rm = TRUE) * 1.1)) +
        # 吧GO term 贴到Bar上
        geom_text(data = page5_plot_data_df,                         
                  aes(x = input$page5_horizontal_barplot_geom_text1_x,
                      y = .data[[input$page5_horizontal_barplot_y]],
                      label = .data[[input$page5_horizontal_barplot_y]]),
                  color = input$page5_horizontal_barplot_geom_text1_colour,
                  size = input$page5_horizontal_barplot_geom_text1_size,
                  hjust = input$page5_horizontal_barplot_geom_text1_hjust,
                  vjust = input$page5_horizontal_barplot_geom_text1_vjust,
                  fontface = input$page5_horizontal_barplot_geom_text1_Font,
                  family = "sans") +
        
        # 标签设置
        labs(x = input$page5_horizontal_barplot_labx,
             y = input$page5_horizontal_barplot_laby,
             title = input$page5_horizontal_barplot_title,
             subtitle = input$page5_horizontal_barplot_subtitle) +
        
        theme_classic() +
        theme(
          axis.title = element_text(size = input$page5_horizontal_barplot_axis.title_size,
                                    face = input$page5_horizontal_barplot_axis.title_Font,
                                    family = "sans",
                                    color = input$page5_horizontal_barplot_axis.title_colour),
          plot.title = element_text(size = input$page5_horizontal_barplot_plot.title_size,
                                    hjust = input$page5_horizontal_barplot_plot.title_hjust,
                                    face = input$page5_horizontal_barplot_plot.title_Font,
                                    family = "sans",
                                    color = input$page5_horizontal_barplot_plot.title_colour),
          plot.subtitle = element_text(size = input$page5_horizontal_barplot_plot.subtitle_size,
                                       hjust = input$page5_horizontal_barplot_plot.subtitle_hjust,
                                       face = input$page5_horizontal_barplot_plot.subtitle_Font,
                                       family = "sans",
                                       color = input$page5_horizontal_barplot_plot.subtitle_colour),
          legend.title = element_text(size = input$page5_horizontal_barplot_axis.title_size,
                                      face = input$page5_horizontal_barplot_axis.title_Font,
                                      family = "sans",
                                      color = input$page5_horizontal_barplot_axis.title_colour),
          legend.text = element_text(size = input$page5_horizontal_barplot_axis.text_size,
                                     face = input$page5_horizontal_barplot_axis.text_Font,
                                     family = "sans",
                                     color = input$page5_horizontal_barplot_axis.text_colour),
          # legend.position = 'right',
          axis.line = element_line(linewidth = input$page5_horizontal_barplot_panel.border_linewidth, 
                                   color = input$page5_horizontal_barplot_panel.border_colour),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = input$page5_horizontal_barplot_axis.text_size,
                                     face = input$page5_horizontal_barplot_axis.text_Font,
                                     family = "sans",
                                     color = input$page5_horizontal_barplot_axis.text_colour),
          axis.ticks.x = element_line(linewidth = input$page5_horizontal_barplot_panel.border_linewidth,
                                      color = input$page5_horizontal_barplot_panel.border_colour),
          strip.placement = "outside"
        )
      
      #### 添加基因ID文本（如果需要）
      if (as.logical(input$page5_horizontal_barplot_show_geneid)) {
        # 固定颜色
        if (!as.logical(input$page5_horizontal_barplot_geom_text2_colour_by_value)) {
          
          p <- p +
            geom_text(data = page5_plot_data_df,
                      show.legend = FALSE,
                      aes(x = input$page5_horizontal_barplot_geom_text2_x, 
                          y = Description, 
                          label = .data[[input$page5_horizontal_barplot_geneid_col]],
                          color = geneid_color),
                      colour = input$page5_horizontal_barplot_geom_text2_colour,
                      size = input$page5_horizontal_barplot_geom_text2_size,
                      fontface = input$page5_horizontal_barplot_geom_text2_Font,
                      hjust = input$page5_horizontal_barplot_geom_text2_hjust,
                      vjust = input$page5_horizontal_barplot_geom_text2_vjust,
                      family = "sans")
        } else {
          p <- p +
            geom_text(data = page5_plot_data_df,
                      show.legend = FALSE,
                      aes(x = input$page5_horizontal_barplot_geom_text2_x, 
                          y = Description, 
                          label = .data[[input$page5_horizontal_barplot_geneid_col]],
                          color = geneid_color),
                      size = input$page5_horizontal_barplot_geom_text2_size,
                      fontface = input$page5_horizontal_barplot_geom_text2_Font,
                      hjust = input$page5_horizontal_barplot_geom_text2_hjust,
                      vjust = input$page5_horizontal_barplot_geom_text2_vjust,
                      family = "sans") +
            
            # 如果需要按值着色，添加颜色映射
            if (is.numeric(page5_plot_data_df$geneid_color)) {
              scale_color_distiller(#name = geneid_lab,
                palette = input$page5_horizontal_barplot_barfill_palette, 
                direction = as.numeric(input$page5_horizontal_barplot_barfill_palette_direction))
              
            } else {
              scale_color_manual(values = c(input$page5_horizontal_barplot_geom_text2_colour1,
                                            input$page5_horizontal_barplot_geom_text2_colour2,
                                            input$page5_horizontal_barplot_geom_text2_colour3,
                                            input$page5_horizontal_barplot_geom_text2_colour4))
            }
        }
      }
      
      #### 添加分面（如果需要）
      if (as.logical(input$page5_horizontal_barplot_facet)) {
        # 确保加载了ggh4x包
        p <- p +
          ggh4x::facet_grid2(rows = vars(.data[[input$page5_horizontal_barplot_facet_by]]),
                             scales = "free", 
                             space = "free",
                             strip = ggh4x::strip_themed(
                               text_y = element_text(size = input$page5_horizontal_barplot_facet_size, 
                                                     face = input$page5_horizontal_barplot_facet_Font,
                                                     family = "sans"),
                               background_y = ggh4x::elem_list_rect(
                                 colour = c(input$page5_horizontal_barplot_facet_border_colour1,
                                            input$page5_horizontal_barplot_facet_border_colour2,
                                            input$page5_horizontal_barplot_facet_border_colour3,
                                            input$page5_horizontal_barplot_facet_border_colour4),
                                 fill = c(input$page5_horizontal_barplot_facet_fill_colour1,
                                          input$page5_horizontal_barplot_facet_fill_colour2,
                                          input$page5_horizontal_barplot_facet_fill_colour3,
                                          input$page5_horizontal_barplot_facet_fill_colour4))
                             ))
      }
      
      if (as.logical(input$page5_horizontal_barplot_barfill_use_palette) && !is_fill_continuous) {
        return(ggplot() + 
                 geom_text(aes(x = 0.5, y = 0.5, 
                               label = "Man！\n你的Bar填充映射值为非连续性变量\n无法使用预设调色板\n请在Bar参数设置里切换手动配色捏~~"), 
                           size = 6) +
                 theme_void())
      } else {
        return(p)
      }
      
      
      ########--------纵向Barplot--------########
    } else if (input$page5_enrichplottype == 'page5_vertical_barplot') {
      
      # 预先geom_bar计算X轴数据
      if (input$page5_vertical_barplot_y %in% c('pvalue', 'qvalue','p.adjust')) {
        page5_plot_data_df$y_value <- -log10(page5_plot_data_df[[input$page5_vertical_barplot_y]])
        y_lab <- paste0("-log10(", input$page5_vertical_barplot_y, ")")
      } else {
        page5_plot_data_df$y_value <- page5_plot_data_df[[input$page5_vertical_barplot_y]]
        y_lab <- input$page5_vertical_barplot_y
      }
      
      # 预先计算geom_bar填充颜色数据
      if (input$page5_vertical_barplot_fill %in% c('pvalue', 'qvalue','p.adjust')) {
        page5_plot_data_df$fill_value <- -log10(page5_plot_data_df[[input$page5_vertical_barplot_fill]])
        fill_lab <- paste0("-log10(", input$page5_vertical_barplot_fill, ")")
      } else {
        page5_plot_data_df$fill_value <- page5_plot_data_df[[input$page5_vertical_barplot_fill]]
        fill_lab <- input$page5_vertical_barplot_fill
      }
      
      # 检测geom_bar的fill变量类型(数值型且唯一变量在4种以上)
      is_fill_continuous <- is.numeric(page5_plot_data_df$fill_value) && 
        length(unique(page5_plot_data_df$fill_value[!is.na(page5_plot_data_df$fill_value)])) > 4
      
      # 预先计算label_color
      if (input$page5_vertical_barplot_bar_label %in% c('pvalue', 'qvalue','p.adjust')) {
        page5_plot_data_df$bar_label_value <- -log10(page5_plot_data_df[[input$page5_vertical_barplot_bar_label]])
        bar_label <- paste0("-log10(", input$page5_vertical_barplot_bar_label, ")")
      } else {
        page5_plot_data_df$bar_label_value <- page5_plot_data_df[[input$page5_vertical_barplot_bar_label]]
        bar_label <- input$page5_vertical_barplot_bar_label
      }
      
      # 绘图
      {if (!as.logical(input$page5_vertical_barplot_use_fill)) {
        # 情况1：不使用fill映射，直接指定一个颜色
        p <- ggplot() +
          geom_bar(data = page5_plot_data_df,
                   aes(x = .data[[input$page5_vertical_barplot_x]],
                       y = y_value),
                   fill = input$page5_vertical_barplot_fill_single_colour,  # 单一颜色
                   width = input$page5_vertical_barplot_bar_width,
                   alpha = input$page5_vertical_barplot_bar_alpha,
                   colour = input$page5_vertical_barplot_bar_colour,
                   linewidth = input$page5_vertical_barplot_bar_linewidth,
                   just = 1-input$page5_vertical_barplot_bar_hjust,
                   show.legend = FALSE,  # 单色不需要图例
                   stat = 'identity')
      } else {
        p <- ggplot() +
          geom_bar(data = page5_plot_data_df,
                   aes(
                     x = .data[[input$page5_vertical_barplot_x]],
                     y = y_value,
                     fill = fill_value
                   ),
                   width = input$page5_vertical_barplot_bar_width,
                   alpha = input$page5_vertical_barplot_bar_alpha,
                   colour = input$page5_vertical_barplot_bar_colour,
                   linewidth = input$page5_vertical_barplot_bar_linewidth,
                   just = 1-input$page5_vertical_barplot_bar_hjust,
                   show.legend = as.logical(input$page5_vertical_barplot_show_legend),
                   stat = 'identity') +
          # 填充颜色设置
          {if (as.logical(input$page5_vertical_barplot_barfill_use_palette) && is_fill_continuous) {
            scale_fill_distiller(name = fill_lab,
                                 palette = input$page5_vertical_barplot_barfill_palette, 
                                 direction = as.numeric(input$page5_vertical_barplot_barfill_palette_direction))
          } else if (!as.logical(input$page5_vertical_barplot_barfill_use_palette) && is_fill_continuous) {
            scale_fill_gradient(name = fill_lab,
                                low = input$page5_vertical_barplot_fill_colour2,
                                high = input$page5_vertical_barplot_fill_colour1)
          } else {
            scale_fill_manual(name = fill_lab,
                              values = c(input$page5_vertical_barplot_fill_colour1,
                                         input$page5_vertical_barplot_fill_colour2,
                                         input$page5_vertical_barplot_fill_colour3,
                                         input$page5_vertical_barplot_fill_colour4))
          }}
      }}
      p <- p +
        # 设置x轴与y轴间隔为0
        scale_y_continuous(expand = c(0,0),
                           limits = c(0, max(if (input$page5_vertical_barplot_y %in% c('pvalue', 'qvalue','p.adjust')) {
                             -log10(page5_plot_data_df[[input$page5_vertical_barplot_y]])
                           } else {
                             page5_plot_data_df[[input$page5_vertical_barplot_y]]
                           }, na.rm = TRUE) * 1.2))
      # Bar_label
      if (as.logical(input$page5_vertical_barplot_show_bar_label)) {
        p <- p +
          geom_text(data = page5_plot_data_df,                         
                    aes(x = .data[[input$page5_vertical_barplot_x]],
                        y = y_value,
                        label = round(bar_label_value,digits = 2)),
                    color = input$page5_vertical_barplot_geom_text1_colour,
                    size = input$page5_vertical_barplot_geom_text1_size,
                    hjust = -input$page5_vertical_barplot_geom_text1_hjust,
                    vjust = -input$page5_vertical_barplot_geom_text1_vjust,
                    fontface = input$page5_vertical_barplot_geom_text1_Font,
                    angle = as.numeric(input$page5_vertical_barplot_geom_text1_angle),
                    family = "sans")
      }
      # 把FoldEnrichment贴到Bar上
      p <- p +
        # 标签设置
        labs(x = input$page5_vertical_barplot_labx,
             y = input$page5_vertical_barplot_laby,
             title = input$page5_vertical_barplot_title,
             subtitle = input$page5_vertical_barplot_subtitle) +
        
        theme_classic() +
        theme(
          axis.title = element_text(size = input$page5_vertical_barplot_axis.title_size,
                                    face = input$page5_vertical_barplot_axis.title_Font,
                                    family = "sans",
                                    color = input$page5_vertical_barplot_axis.title_colour),
          plot.title = element_text(size = input$page5_vertical_barplot_plot.title_size,
                                    hjust = input$page5_vertical_barplot_plot.title_hjust,
                                    face = input$page5_vertical_barplot_plot.title_Font,
                                    family = "sans",
                                    color = input$page5_vertical_barplot_plot.title_colour),
          plot.subtitle = element_text(size = input$page5_vertical_barplot_plot.subtitle_size,
                                       hjust = input$page5_vertical_barplot_plot.subtitle_hjust,
                                       face = input$page5_vertical_barplot_plot.subtitle_Font,
                                       family = "sans",
                                       color = input$page5_vertical_barplot_plot.subtitle_colour),
          legend.title = element_text(size = input$page5_vertical_barplot_axis.title_size,
                                      face = input$page5_vertical_barplot_axis.title_Font,
                                      family = "sans",
                                      color = input$page5_vertical_barplot_axis.title_colour),
          legend.text = element_text(size = input$page5_vertical_barplot_axis.text_size,
                                     face = input$page5_vertical_barplot_axis.text_Font,
                                     family = "sans",
                                     color = input$page5_vertical_barplot_axis.text_colour),
          # legend.position = 'right',
          axis.line = element_line(linewidth = input$page5_vertical_barplot_panel.border_linewidth, 
                                   color = input$page5_vertical_barplot_panel.border_colour),
          axis.text.x = element_text(size = input$page5_vertical_barplot_axis.text_size,
                                     face = input$page5_vertical_barplot_axis.text_Font,
                                     family = "sans",
                                     color = input$page5_vertical_barplot_axis.text_colour,
                                     angle = as.numeric(input$page5_vertical_barplot_axis.text_angle),
                                     hjust = input$page5_vertical_barplot_axis.text_hjust),
          axis.ticks.x = element_line(linewidth = input$page5_vertical_barplot_panel.border_linewidth,
                                      color = input$page5_vertical_barplot_panel.border_colour),
          axis.text.y = element_text(size = input$page5_vertical_barplot_axis.text_size,
                                     face = input$page5_vertical_barplot_axis.text_Font,
                                     family = "sans",
                                     color = input$page5_vertical_barplot_axis.text_colour), # y轴刻度文字
          axis.ticks.y = element_line(linewidth = input$page5_vertical_barplot_panel.border_linewidth,
                                      color = input$page5_vertical_barplot_panel.border_colour), # y轴刻度线
          strip.placement = "outside",
          plot.margin = margin(input$page5_vertical_barplot_plot.margin_up, 
                               input$page5_vertical_barplot_plot.margin_right, 
                               input$page5_vertical_barplot_plot.margin_down, 
                               input$page5_vertical_barplot_plot.margin_left, 
                               "pt")# 增加边距（上、右、下、左）单位可以是 "pt", "mm", "cm", "in"
        )
      
      #### 添加分面（如果需要）
      if (as.logical(input$page5_vertical_barplot_facet)) {
        # 确保加载了ggh4x包
        p <- p +
          ggh4x::facet_grid2(cols = vars(.data[[input$page5_vertical_barplot_facet_by]]),
                             scales = "free", 
                             space = "free",
                             strip = ggh4x::strip_themed(
                               text_x = element_text(size = input$page5_vertical_barplot_facet_size, 
                                                     face = input$page5_vertical_barplot_facet_Font,
                                                     family = "sans"),
                               background_x = ggh4x::elem_list_rect(
                                 colour = c(input$page5_vertical_barplot_facet_border_colour1,
                                            input$page5_vertical_barplot_facet_border_colour2,
                                            input$page5_vertical_barplot_facet_border_colour3,
                                            input$page5_vertical_barplot_facet_border_colour4),
                                 fill = c(input$page5_vertical_barplot_facet_fill_colour1,
                                          input$page5_vertical_barplot_facet_fill_colour2,
                                          input$page5_vertical_barplot_facet_fill_colour3,
                                          input$page5_vertical_barplot_facet_fill_colour4))
                             ))
      }
      
      if (as.logical(input$page5_vertical_barplot_barfill_use_palette) && !is_fill_continuous) {
        return(ggplot() + 
                 geom_text(aes(x = 0.5, y = 0.5, 
                               label = "Man！\n你的Bar填充映射值为非连续性变量\n无法使用预设调色板\n请在Bar参数设置里切换手动配色捏~~"), 
                           size = 6) +
                 theme_void())
      } else {
        return(p)
      }
      
      ########--------富集聚类图--------########
    } else if (input$page5_enrichplottype == 'page5_cluster_enrichplot') {
      
      # 检查输入的colorBy与nodeSize列是否存在于数据框中
      if (!input$page5_cluster_enrich_colorBy %in% colnames(page5_plot_data_df) || !input$page5_cluster_enrich_nodeSize %in% colnames(page5_plot_data_df)) {
        p <- ggplot() + 
          geom_text(aes(x = 0.5, y = 0.5, 
                        label = "Oi ！\n你输入的着色列或点大小映射列不在你的数据里！\n请仔细检查！"), 
                    size = 6) +
          theme_void()
      } else {
        p <- enrichmentNetwork(page5_plot_data_df,
                               simMethod = input$page5_cluster_enrich_simMethod,  # 设置相似性计算方法
                               clustMethod = input$page5_cluster_enrich_clustMethod,  # 设置聚类方法
                               clustNameMethod = input$page5_cluster_enrich_clustNameMethod,  # 设置聚类名称方法
                               innerCutoff = input$page5_cluster_enrich_innerCutoff,                              # 设置同一聚类内节点之间的最低相似性阈值，只有相似性超过该值的节点才会归为同一聚类。
                               outerCutoff = input$page5_cluster_enrich_outerCutoff,                          # 定义不同聚类之间的相似性阈值，用于区分聚类边界并控制网络中跨聚类连接的显示。
                               colorBy= input$page5_cluster_enrich_colorBy,                       
                               colorType= input$page5_cluster_enrich_colorType,                        
                               nodeSize= input$page5_cluster_enrich_nodeSize,                        
                               fontSize= input$page5_cluster_enrich_fontSize,                        
                               drawEllipses= as.logical(input$page5_cluster_enrich_drawEllipses),                        
                               pCutoff= input$page5_cluster_enrich_pCutoff,                       
                               verbose= TRUE,
                               minClusterSize = input$page5_cluster_enrich_minClusterSize,  # 设置最小聚类通路数
                               repelLabels = as.logical(input$page5_cluster_enrich_repelLabels)
        ) +
          labs(title = input$page5_cluster_enrich_title,
               subtitle = input$page5_cluster_enrich_subtitle) +
          guides(size = guide_legend(title = input$page5_cluster_enrich_nodeSize)) +
          theme(legend.position = "right",
                legend.title = element_text(color = input$page5_cluster_enrich_legend.title_colour, 
                                            size = input$page5_cluster_enrich_legend.title_size,
                                            face = input$page5_cluster_enrich_legend.title_Font,
                                            family = "sans"
                ),
                legend.text = element_text(color = input$page5_cluster_enrich_legend.text_colour, 
                                           size = input$page5_cluster_enrich_legend.text_size,
                                           face = input$page5_cluster_enrich_legend.text_Font
                ),
                plot.title = element_text(color = input$page5_cluster_enrich_plot.title_colour,
                                          size = input$page5_cluster_enrich_plot.title_size,
                                          hjust= input$page5_cluster_enrich_plot.title_hjust,
                                          face= input$page5_cluster_enrich_plot.title_Font,
                                          family = "sans"
                ),
                plot.subtitle = element_text(color = input$page5_cluster_enrich_plot.subtitle_colour,
                                             size = input$page5_cluster_enrich_plot.subtitle_size,
                                             hjust= input$page5_cluster_enrich_plot.subtitle_hjust,
                                             face= input$page5_cluster_enrich_plot.subtitle_Font,
                                             family = "sans"
                ))
        # 设置颜色映射
        if (input$page5_cluster_enrich_use_palette == 'TRUE') {
          p <- p + 
            scale_color_continuous_c4a_seq(input$page5_cluster_enrich_fill_palette, 
                                           reverse = as.logical(input$page5_cluster_enrich_fill_palette_direction))
        } else {
          p <- p + 
            scale_color_gradient(low = input$page5_cluster_enrich_fill_colour1,
                                 high = input$page5_cluster_enrich_fill_colour2)
        } 
      }
      
      return(p)
      
      
      
      
      ########--------环形富集图--------########
    } else if (input$page5_enrichplottype == 'page5_circlize_enrichplot') {
      return(NULL)
    } else {
      return(NULL)
    }
  })
  
  
  ## 输入数据展示
  output$page5_text <- DT::renderDT({
    page5_raw <- input$page5_EnrichFile
    if (is.null(page5_raw)) return(NULL)
    page5_df <- read.delim(file = page5_raw$datapath, 
                           header = input$page5_header, 
                           stringsAsFactors = FALSE, 
                           sep = input$page5_sep, 
                           encoding = 'UTF-8')
    return(page5_df)
  }, options = list(pageLength = 5))
  
  
  # 图片输出框
  output$page5_plot <- renderPlot({
    if (is.null(page5_plot_obj())) return(NULL)
    page5_plot_obj()
  },
  width = function() input$page5_plot_panel_width,
  height = function() input$page5_plot_panel_height,
  res = 96)
  # 图片下载框
  output$page5_plotdownload <- downloadHandler(
    filename = function() {
      paste(input$page5_enrichplottype, Sys.Date(), '.', input$page5_extPlot, sep = '')
    },
    content = function(file) {
      ggsave(filename = file, plot = page5_plot_obj(),
             width = as.numeric(input$page5_plotwidth),
             height = as.numeric(input$page5_plotheight), dpi = 300)
    }
  )
  
  
  
  ##################################  FUNC 6   #################################
  page6patternresult <- eventReactive(input$page6searchstart, {
    # withProgress(message = 'Calculation in progress',
    #              detail = 'This may take a while...', value = 0, {
    #                for (i in 1:15) {
    #                  incProgress(1/15)
    #                  Sys.sleep(0.25)
    #                }
    #              })
    if (input$page6geneIDInputMethod == "file") {
      req(input$page6IDlist)
      page6patterns <- readLines(input$page6IDlist$datapath)
      page6patterns <- trimws(page6patterns[page6patterns != ""])
      if (length(page6patterns) == 0) return(NULL)
    } else {
      req(input$page6IDText)
      page6patterns <- unlist(strsplit(input$page6IDText, "\n"))
      page6patterns <- trimws(page6patterns[page6patterns != ""])
      if (length(page6patterns) == 0) return(NULL)
    }
    
    if (input$page6species == "osativa") {
      data <- os_data
    }else{
      data <- ath_data
    }
    # matched_rows <- data %>%
    #   rowwise() %>%
    #   filter(any(c_across(everything()) %in% page6patterns)) %>%
    #   ungroup()
    matched_rows <- data %>%
      filter(if_any(everything(), ~ . %in% page6patterns))
    
    matched_rows
  })
  
  output$page6table1 <- renderDT({
    page6result <- page6patternresult()
    if (is.null(page6result)) return(NULL)
    
    datatable(as.data.frame(page6result),
              options = list(pageLength = 20, scrollX = TRUE),
              rownames = FALSE)
  }) 
  output$page6tabledownloadData <- downloadHandler(
    filename = function() {
      paste("ID_search_results_", Sys.Date(), ".txt")
    },
    content = function(file) {
      write.table(as.data.frame(page6patternresult()), file, row.names = FALSE,quote = F,col.names = T,sep = "\t")
    }
  )
  ####################################FUN8######################################
  values <- reactiveValues(
    showImage2 = FALSE
  )
  
  observeEvent(input$textLink2, {
    values$showImage2 <- TRUE
    runjs("setTimeout(function() { window.open('https://www.bilibili.com/video/BV1GJ411x7h7', '_blank'); }, 1500);")
  })
  
  output$showImage2 <- reactive({
    values$showImage2
  })
  outputOptions(output, "showImage2", suspendWhenHidden = FALSE)
  output$displayImage2 <- renderUI({
    tags$img(
      src = "https://raw.githubusercontent.com/EdmundFieldQIN/WgroupBioinfoToolBoxShinyApp/refs/heads/main/zhenghuo.jpg",
      width = "600px"  # 可选：指定图片大小
    )
  })
  
  ##################################  FUNC 9   #################################
  ## 读取raw_qrt-pcr数据
  page9_raw_df <- reactive({
    req(input$page9_qrtpcr_file)
    if (is.null(input$page9_qrtpcr_file)) {
      return(NULL)
    }
    # 自动检测编码（取第一个可能性）
    detected_encoding <- readr::guess_encoding(input$page9_qrtpcr_file$datapath, n_max = 1000)$encoding[1]
    
    # 使用 readLines() + 指定 encoding读取所有行
    all_lines <- readLines(file(input$page9_qrtpcr_file$datapath, encoding = detected_encoding))
    
    # 查找包含关键字段（比如表头“GeneID”）的那一行
    header_line_index <- which(grepl("样品名称|Sample", all_lines))[1]
    if (is.na(header_line_index)) {
      showNotification("未找到表头行，请检查文件格式或内容。", type = "error")
      return(NULL)
    }
    
    # 读取该行及其后内容为新的数据框
    page9_df <- read.table(file = file(input$page9_qrtpcr_file$datapath, encoding = detected_encoding),
                           skip = header_line_index - 1,  # 因为read.table的skip是跳过多少行
                           header = input$page9_header,
                           sep = input$page9_sep,      # 可改为 "," 如果是csv文件
                           stringsAsFactors = FALSE,
                           check.names = FALSE  # ⚠️ 禁用 make.names，防止乱码列名
    )
    
    # 替换中文列名为英文（仅当列名包含中文）
    if ("样品名称" %in% colnames(page9_df)) {
      colnames(page9_df)[colnames(page9_df) == "样品名称"] <- "Sample"
    }
    if ("目的基因" %in% colnames(page9_df)) {
      colnames(page9_df)[colnames(page9_df) == "目的基因"] <- "Gene"
    }
    if ("目的基因Ct" %in% colnames(page9_df)) {
      colnames(page9_df)[colnames(page9_df) == "目的基因Ct"] <- "Ct"
    }
    
    # 查看结果
    page9_df <- page9_df %>% 
      dplyr::select(Sample, Gene, Ct)
    
    # 将 目的基因Ct 转为数值
    page9_df$Ct <- as.numeric(page9_df$Ct)
    
    # 过滤掉非数值（NA）行
    page9_df <- page9_df %>%
      dplyr::filter(!is.na(Ct))
    
    
    return(page9_df)
  })
  
  
  # 计算qRT-PCR结果
  page9_calculate_results <- eventReactive(input$page9_calculate_start, {
    req(page9_raw_df())
    if (is.null(page9_raw_df())) {
      return(NULL)
    }
    page9_qrtpcr_df <- page9_raw_df()
    # step1：计算每个样品中 actin 的平均 Ct
    reference_ct_df <- page9_qrtpcr_df %>%
      dplyr::filter(Gene == input$page9_reference_gene) %>%
      dplyr::group_by(Sample) %>%
      dplyr::summarise(Ref_Ct = mean(Ct, na.rm = TRUE))
    # step2：将内参基因Ct 加回原数据框
    page9_qrtpcr_df <- page9_qrtpcr_df %>%
      dplyr::left_join(reference_ct_df, by = "Sample")
    
    # Step3: 计算ΔCt
    page9_qrtpcr_df <- page9_qrtpcr_df %>%
      dplyr::mutate(
        ΔCt = Ct - Ref_Ct
      )
    # step4: 计算Reference_ΔCt
    reference_Δct_df <- page9_qrtpcr_df %>%
      dplyr::filter(Sample == input$page9_control_name) %>%
      dplyr::group_by(Gene) %>%
      dplyr::summarise(`Reference_ΔCt` = mean(ΔCt, na.rm = TRUE))
    # step5：将Reference_ΔCt加回原数据框
    page9_qrtpcr_df <- page9_qrtpcr_df %>%
      dplyr::left_join(reference_Δct_df, by = "Gene")
    # step6：计算ΔΔCt和 2^(-ΔΔCt)
    page9_qrtpcr_df <- page9_qrtpcr_df %>%
      dplyr::mutate(
        ΔΔCt = ΔCt - `Reference_ΔCt`,
        `2^(-ΔΔCt)` = 2^(-ΔΔCt)
      )
    
    
    # Step7: 计算 ZH11 每个基因的参考表达量（平均 2^(-ΔΔCt)）
    reference_fc <- page9_qrtpcr_df %>%
      dplyr::filter(Sample == input$page9_control_name) %>%
      dplyr::group_by(Gene) %>%
      dplyr::summarise(`reference_2^(-ΔΔCt)` = mean(`2^(-ΔΔCt)`, na.rm = TRUE), .groups = "drop")
    
    # Step8: 合并并计算 FC（当前表达 / 参考表达）
    page9_qrtpcr_df <- page9_qrtpcr_df %>%
      dplyr::left_join(reference_fc, by = "Gene") %>%
      dplyr::mutate(FC = `2^(-ΔΔCt)` / `reference_2^(-ΔΔCt)`) %>% 
      dplyr::group_by(Sample, Gene) %>%
      dplyr::mutate(average_FC = mean(FC, na.rm = TRUE))
    
    return(page9_qrtpcr_df)
  })
  
  
  
  ## 下载计算结果
  # 下载结果
  output$page9_download_data_all <- downloadHandler(
    filename = function() {
      paste(Sys.Date(),"_qRT-pcr_all_results.csv", sep = "")
    },
    content = function(file) {
      write.table(
        as.data.frame(page9_calculate_results()),
        file, row.names = FALSE,quote = F,sep = ",",col.names = T,fileEncoding = "GB18030")
    }
  )
  
  output$page9_download_data_only_fc <- downloadHandler(
    filename = function() {
      paste(Sys.Date(),"_qRT-pcr_only_FC_results.csv", sep = "")
    },
    content = function(file) {
      write.table(
        as.data.frame(page9_calculate_results()) %>% 
          dplyr::filter(!Gene == input$page9_reference_gene) %>%
          dplyr::select(Sample, Gene, FC, average_FC)
        , file, row.names = FALSE,quote = F,sep = ",",col.names = T,fileEncoding = "GB18030")
    }
  )
  
  
  
  ## 输入数据展示
  output$page9_input_data <- DT::renderDT({
    req(page9_raw_df())
    datatable(page9_raw_df(), options = list(pageLength = 6))
  })
  
  output$page9_output_data_all <- DT::renderDT({
    req(page9_calculate_results())
    datatable(page9_calculate_results(), options = list(pageLength = 5))
  })
  
  output$page9_output_data_only_fc <- DT::renderDT({
    req(page9_calculate_results())
    page9_qrtpcr_df1 <- page9_calculate_results()
    # 只保留样品名称、目的基因和FC列
    page9_qrtpcr_df_only_fc <- page9_qrtpcr_df1 %>%
      dplyr::filter(!Gene == input$page9_reference_gene) %>%
      dplyr::select(Sample, Gene, FC, average_FC)
    datatable(page9_qrtpcr_df_only_fc, options = list(pageLength = 5))
  })
  
  
  
  
  ################################设置示例数据##################################
  ###### page1 示例数据
  ###设置示例数据
  test1<-matrix(sample(0:100,size = 60),10,6)
  rownames(test1)<-paste("AT1G0",c(101:110),"0",sep = "")
  colnames(test1)<-c(paste("Control",c(1:3),sep = ""),paste("Treat",c(1:3),sep = ""))
  test1 <- as.data.frame(test1) %>% 
    rownames_to_column(var = "gene_id")
  
  test2<-c(rep("Control",3),rep("Treat",3))
  
  #通过点击按钮，可以弹出界面，这个界面可以显示ui的输出界面，因此要做tagList中添加ui输出控件
  #弹窗一
  observeEvent(input$win1, {
    shinyalert(
      html = TRUE,
      text = tagList(
        "FeatureCounts Data Format",
        tableOutput("example1"),
      ),
      size = "l",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE)
  })
  
  ##关联示例控件的函数
  output$example1 <- renderTable({
    test1
  },rownames = T)
  
  #弹窗2及关联函数
  observeEvent(input$win2, {
    shinyalert(
      html = TRUE,
      text = tagList(
        "Condition Data Format",
        tableOutput("example2")
      ),
      size = "l",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE)
  })
  
  output$example2 <- renderTable({
    head(test2)
  },rownames = F,colnames = F)
  
  
  
  
  ###### page2 示例数据
  ##示例文件
  
  rawMat<-matrix(sample(0:100,size = 18),3,6)
  rownames(rawMat)<-paste("AT1G0",c(101:103),"0",sep = "")
  colnames(rawMat)<-c(paste("Control",c(1:3),sep = ""),paste("Treat",c(1:3),sep = ""))
  rawFC<-data.frame(chr=c(rep("Chr1",3)),start=c(1250,25966,35689),end=c(1350,27976,38689),strand=c(rep("+",3)),Length=c(1520,569,6541))
  rawFC<-cbind(rawFC,as.data.frame(rawMat))
  
  #ui输出控件
  observeEvent(input$winht, {
    shinyalert(
      html = TRUE,
      text = tagList(
        "Raw FeatureCounts Data Format",
        tableOutput("raw_example"),
        "Matrix Data Format",
        tableOutput("mat_example")
      ),
      size = "l",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE)
  })
  
  ##示例控件的函数
  output$raw_example <- renderTable({
    head(rawFC)
  },rownames = T)
  
  output$mat_example <- renderTable({
    head(rawMat)
  },rownames = T)
  

  ########## page3 示例数据
  page3_example_data1 <- data.frame(gene_id = c(paste("AT1G0",c(101:105),"0",sep = "")),
                                    log2foldchange = rnorm(5, mean = 0, sd = 2),
                                    pvalue = runif(5, min = 0, max = 0.2)
  )
  page3_example_data2 <- data.frame(gene_id = c(paste("AT1G0",c(101:105),"0",sep = "")),
                                    log2foldchange_1 = rnorm(5, mean = 0, sd = 2),
                                    pvalue_1 = runif(5, min = 0, max = 0.2),
                                    log2foldchange_2 = rnorm(5, mean = 0, sd = 3),
                                    pvalue_2 = runif(5, min = 0, max = 0.2)
  )
  
  #page3弹窗
  observeEvent(input$page3_showexampledata1, {
    shinyalert(
      html = TRUE,
      text = tagList(
        "普通火山图输入数据格式",
        tableOutput("page3_example1"),
      ),
      size = "s",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE
      )
  })
  
  observeEvent(input$page3_showexampledata2, {
    shinyalert(
      html = TRUE,
      text = tagList(
        "九象限火山图单独输入数据格式",
        tableOutput("page3_example1"),
        "九象限合并后输入数据格式",
        tableOutput("page3_example2"),
      ),
      size = "l",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE
      )
  })
  #关联示例控件的函数
  output$page3_example1 <- renderTable({
    page3_example_data1
  },rownames = F,colnames = T)
  output$page3_example2 <- renderTable({
    page3_example_data2
  },rownames = F,colnames = T)
  
  
  
  
  ########### page4 示例数据
  gene2term2name <- data.frame(gene_id = c(paste("AT1G0",c(101:103),"0",sep = "")),
                               GO = c(paste("GO:0000",c(411:413),"0",sep = "")),
                               Description = c("tRNA binding","exocyst","Golgi membrane")
  )
  # page4弹窗
  observeEvent(input$page4_showexampledata, {
    shinyalert(
      html = TRUE,
      text = tagList(
        "Gene2Term2Name Data Format",
        tableOutput("page4_example"),
      ),
      size = "l",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE
      )
  })
  # page4关联示例控件的函数
  output$page4_example <- renderTable({
    gene2term2name
  },rownames = F,colnames = T)
  
  
  
  ##############  page5 示例数据
  page5_example_data <- read.table(system.file("extdata/page5_sample_data.txt", package = "org.Osativa.eg.db"), 
                                   sep = "\t", header = T,row.names = NULL)
  
  #page5弹窗
  observeEvent(input$page5_showexampledata, {
    shinyalert(
      html = TRUE,
      text = tagList(
        "富集结果输入数据格式",
        tableOutput("page5_example"),
      ),
      size = "l")
  })
  #关联示例控件的函数
  output$page5_example <- renderTable({
    page5_example_data
  },rownames = F,colnames = T)
  
  
  
  ####################### page9 示例数据
  page9_example_data <- data.frame(
    Sample = rep(c("ZH11", "Sample1"), each = 4),
    Gene = rep(c("actin", "actin", "gene1", "gene1"), 2),
    Ct = c(20.1, 22.3,15.2, 15.5, 19.8, 18.7, 15.3, 15.4)
  )
  
  #page9弹窗
  observeEvent(input$page9_showexampledata, {
    shinyalert(
      html = TRUE,
      text = tagList(
        "原始输入数据格式(表头要一致)",
        tableOutput("page9_example"),
      ),
      size = "s")
  })
  #关联示例控件的函数
  output$page9_example <- renderTable({
    page9_example_data
  },rownames = F,colnames = T)
  
  
  
  ################################### close ####################################
  # 新增会话结束监听
  # 本地运行脚本时请注释掉本模块，作为Tbtools插件运行时请打开本模块
  # session$onSessionEnded(function() {
  #   stopApp()
  #   q("no") # 强制退出R进程
  # })
  # 
  
  
  
  
  ##################################clean Fileinput#############################
  ## 清空数据
  #page1
  observeEvent(input$page1_clear_btn, {
    page1_inputs_to_reset <- c(
      "matFile", "conditionFile"
    )
    lapply(page1_inputs_to_reset, function(id) shinyjs::reset(id))
  })
  
  #page2
  observeEvent(input$page2_clear_btn, {
    page2_inputs_to_reset <- c(
      "matrixFile", "colgroupFile", "rowgroupFile", "secondMatFile"
    )
    lapply(page2_inputs_to_reset, function(id) shinyjs::reset(id))
  })
  
  #page3
  observeEvent(input$page3_clear_btn, {
    page3_inputs_to_reset <- c(
      "volcanoFile", "tgvolcanoFile1", "tgvolcanoFile2", "volcanoFile_merged"
    )
    lapply(page3_inputs_to_reset, function(id) shinyjs::reset(id))
  })
  
  #page4
  observeEvent(input$page4_clear_btn, {
    page4_inputs_to_reset <- c(
      "Gene2Term2Name", "enrich_gene_list"
    )
    lapply(page4_inputs_to_reset, function(id) shinyjs::reset(id))
  })
  
  #page5
  observeEvent(input$page5_clear_btn, {
    page5_inputs_to_reset <- c(
      "page5_EnrichFile"
    )
    lapply(page5_inputs_to_reset, function(id) shinyjs::reset(id))
  })
  
  #page6
  observeEvent(input$page6_clear_btn, {
    page6_inputs_to_reset <- c(
      "page6IDlist", "page6IDText"
    )
    lapply(page6_inputs_to_reset, function(id) shinyjs::reset(id))
  })
  
  #page9
  ## 清空输入数据
  observeEvent(input$page9_clear_btn, {
    page9_inputs_to_reset <- c(
      "page9_qrtpcr_file"
    )
    lapply(page9_inputs_to_reset, function(id) shinyjs::reset(id))
  })
  
  }
  

options(shiny.maxRequestSize = 30 * 1024^2)
# Run the application 
shinyApp(ui = ui, server = server
         # ,options = list(launch.browser = TRUE)
         )
