
ui <- fluidPage(
  useShinyjs(),
  
  # DT::DTOutput("blank_tbl"),
  # uiOutput("blank_selectize"),
  
  tags$style("
  .tiny { 
    width: 1px !important;
    height: 1px !important;
    opacity: 0.01;
    position: absolute;
    overflow: hidden;
  }
"),
  
  # Elements to hide
  tags$div(class = "tiny",
           DTOutput("blank_tbl"),
           uiOutput("blank_selectize")
  ),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      .survey-card {
        border: 1px solid #ddd;
        padding: 15px;
        margin-bottom: 15px;
        border-radius: 4px;
      }
      .survey-card:hover {
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .action-buttons {
        margin-top: 10px;
      }
      .create-new-btn {
        margin: 20px 0;
      }
      .last-updated {
        color: #666;
        font-size: 0.9em;
      }
      .input-container {
        padding: 15px;
        background: #f8f9fa;
        margin-bottom: 20px;
        border-radius: 4px;
        display: flex;
        gap: 20px;
      }
      .data-inputs-column {
        flex: 2;
        display: flex;
        flex-direction: column;
        gap: 5px;
      }
      .other-inputs-column {
        flex: 1;
        display: flex;
        flex-direction: column;
        gap: 10px;
      }
      
      .survey-header {
        background: #f8f9fa;
        padding: 15px;
        border-radius: 4px;
        margin-bottom: 20px;
      }
      .survey-header h2 {
        margin: 0;
        color: #2c3e50;
      }
      .text-muted {
        color: #6c757d;
      }
    "))
  ),
  
  navbarPage(
    "Survey Monitor",
    id = "mainNav",
    
    # Homepage tab
    tabPanel(
      "Home",
      div(
        class = "container",
        h1("Survey Monitors"),
        
        # Create new survey button
        div(
          class = "create-new-btn",
          actionButton("createNew", "Create New Survey Monitor", 
                       class = "btn-primary")
        ),
        
        # Active surveys list
        h3("Active Monitors"),
        uiOutput("surveyList")
      )
    ),
    
    # Dashboard tab (initially hidden)
    tabPanel(
      "Dashboard",
      value = "dashboard",
      uiOutput("dashboardUI")
    )
  )
)