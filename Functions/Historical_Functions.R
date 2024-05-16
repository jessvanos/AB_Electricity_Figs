################################################################################  
## FUNCTION: emissions_type
## Plot stacked area of Alberta emissions vs Canada emissions for electricity and total.
##
## INPUTS: 
##    GHG_AB_CAN_Elec - Filtered data
################################################################################
emissions_type <- function(GHG_AB_CAN_Elec) {
  
  # Separate data into categories and combine
  AB_Elec<- GHG_AB_CAN_Elec %>%
    select(.,'Year','AB_Elec') %>%
    rename('GHGs'='AB_Elec') %>%
    mutate('Type'='Electricity',
           'Zone'='Alberta')
  
  AB_Tot<- GHG_AB_CAN_Elec %>%
    select(.,'Year','AB_All') %>%
    rename('GHGs'='AB_All') %>%
    mutate('Type'='Total Emissions',
           'Zone'='Alberta')
  
  CAN_Tot<- GHG_AB_CAN_Elec %>%
    mutate('GHGs'=CAN_Elec-AB_Elec,
           'Type'='Electricity',
           'Zone'='Rest of Canada') %>%
    select(.,'Year','GHGs','Type','Zone')
  
  CAN_OT<- GHG_AB_CAN_Elec %>%
    mutate('GHGs'=CAN_All-AB_All,
           'Type'='Total Emissions',
           'Zone'='Rest of Canada') %>%
    select(.,'Year','GHGs','Type','Zone')
  
  data<-rbind(CAN_Tot,CAN_OT,AB_Elec,AB_Tot)
  data$Zone <- factor(data$Zone, levels=c("Rest of Canada","Alberta"))
  
  txt_sz=12
  
  # Plot together
  
  ggplot(data) +
    geom_area(aes(x = Year, y = GHGs, fill = Zone), colour = "black", 
              alpha=0.8, size=0.25) +
    
    facet_grid(~fct_relevel(Type,'Total Emissions','Electricity')) +
    
    # Set the theme for the plot
    theme_minimal() +
    theme(panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA)) +
    
    # Text and legend
    theme(axis.text.x = element_text(vjust = 1,color="black"),
          axis.text.y = element_text(color="black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size= txt_sz+4, face = "bold"),
          legend.title=element_blank(),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent',colour ='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          legend.position = "bottom",
          plot.caption = element_text(size=txt_sz-2,hjust = 1)) +
    
    
    theme(plot.title = element_text(),
          text = element_text(size= txt_sz),
          panel.spacing = unit(2, "lines"),
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'black'),
          axis.ticks.x = element_line(),
          
    ) +
    scale_y_continuous(expand=c(0,0), limits = c(0,800), 
                       breaks = seq(0, 800, by = 200),labels=comma) +
    
    scale_x_continuous(expand=c(0,0)) +
    
    labs(y = 'GHG Emissions (Mt CO2e)',caption="Data from Environment and Climate Change Canada") +
    
    #Add colour
    scale_fill_manual(values = c('Rest of Canada'="#C9C9C9",'Alberta'="#4472C4")) 
}
################################################################################  
## FUNCTION: emissions_zone
## Plot stacked area of Alberta emissions and Canada emissions seperated by sector.
##
## INPUTS: 
##    GHG_AB_CAN_Elec - Filtered data
################################################################################
emissions_zone <- function(GHG_AB_CAN_Elec) {
  
  #reshape data
  AB_Elec<- GHG_AB_CAN_Elec %>%
    select(.,'Year','AB_Elec') %>%
    rename('GHGs'='AB_Elec') %>%
    mutate('Type'='Electricity',
           'Zone'='Alberta')
  
  AB_OT<- GHG_AB_CAN_Elec %>%
    mutate('GHGs'=AB_All-AB_Elec,
           'Type'='Other Sectors',
           'Zone'='Alberta') %>%
    select(.,'Year','GHGs','Type','Zone')
  
  CAN_Elec<- GHG_AB_CAN_Elec %>%
    select(.,'Year','CAN_Elec') %>%
    rename('GHGs'='CAN_Elec') %>%
    mutate('Type'='Electricity',
           'Zone'='Canada')
  
  CAN_OT<- GHG_AB_CAN_Elec %>%
    mutate('GHGs'=CAN_All-CAN_Elec,
           'Type'='Other Sectors',
           'Zone'='Canada') %>%
    select(.,'Year','GHGs','Type','Zone')
  
  data<-rbind(CAN_Elec,CAN_OT,AB_Elec,AB_OT)
  data$Type <- factor(data$Type, levels=c("Other Sectors","Electricity"))
  
  txt_sz=12
  
  
  ## PLOT WITH AREA PLOT
  
  ggplot(data) +
    geom_area(aes(x = Year, y = GHGs, fill = Type), colour = "black", 
              alpha=0.8, size=0.25) +
    
    facet_grid(~fct_relevel(Zone,'Canada','Alberta')) +
    
    # Set the theme for the plot
    theme_minimal() +
    theme(panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA)) +
    
    # Text and legend
    theme(axis.text.x = element_text(vjust = 1,color="black"),
          axis.text.y = element_text(color="black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size= txt_sz+4, face = "bold"),
          legend.title=element_blank(),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent',colour ='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          legend.position = "right",
          plot.caption = element_text(size=txt_sz-2,hjust = 2)) +
    
    
    theme(plot.title = element_text(),
          text = element_text(size= txt_sz),
          panel.spacing = unit(2, "lines"),
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'black'),
          axis.ticks.x = element_line(),
          
    ) +
    scale_y_continuous(expand=c(0,0), limits = c(0,800), 
                       breaks = seq(0, 800, by = 200),labels=comma) +
    
    scale_x_continuous(expand=c(0,0)) +
    
    labs(y = 'GHG Emissions (Mt CO2e)',caption="Data from Environment and Climate Change Canada") +
    
    #Add colour
    scale_fill_manual(values = c('Other Sectors'="#C9C9C9",'Electricity'="#4472C4")) 
}

################################################################################  
## FUNCTION: emissions_type_2
## Plot stacked area of Alberta emissions vs Canada emissions for electricity and total. Not same axis
##
## INPUTS: 
##    GHG_AB_CAN_Elec - Filtered data
################################################################################
emissions_type_2 <- function(GHG_AB_CAN_Elec) {
  
  # Separate data into categories and combine
  AB_Elec<- GHG_AB_CAN_Elec %>%
    select(.,'Year','AB_Elec') %>%
    rename('GHGs'='AB_Elec') %>%
    mutate('Type'='Electricity',
           'Zone'='Alberta')
  
  AB_Tot<- GHG_AB_CAN_Elec %>%
    select(.,'Year','AB_All') %>%
    rename('GHGs'='AB_All') %>%
    mutate('Type'='Total Emissions',
           'Zone'='Alberta')
  
  CAN_Tot<- GHG_AB_CAN_Elec %>%
    mutate('GHGs'=CAN_Elec-AB_Elec,
           'Type'='Electricity',
           'Zone'='Rest of Canada') %>%
    select(.,'Year','GHGs','Type','Zone')
  
  CAN_OT<- GHG_AB_CAN_Elec %>%
    mutate('GHGs'=CAN_All-AB_All,
           'Type'='Total Emissions',
           'Zone'='Rest of Canada') %>%
    select(.,'Year','GHGs','Type','Zone')
  
  data<-rbind(CAN_Tot,CAN_OT,AB_Elec,AB_Tot)
  data$Zone <- factor(data$Zone, levels=c("Rest of Canada","Alberta"))
  
  txt_sz=16
  
  # Create two new datasets
  All_data<-data %>%
    filter(Type=='Total Emissions')
  E_data<-data %>%
    filter(Type=='Electricity')
  
  
  # Plot together
  
  p1<-ggplot(All_data) +
    geom_area(aes(x = Year, y = GHGs, fill = Zone), colour = "black", 
              alpha=0.8, size=0.25) +
    
    # Set the theme for the plot
    theme_minimal() +
    theme(panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA)) +
    
    # Text and legend
    theme(axis.text.x = element_text(vjust = 1,color="black"),
          axis.text.y = element_text(color="black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size= txt_sz+4, face = "bold"),
          legend.title=element_blank(),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent',colour ='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          legend.position = "bottom",
          legend.text=element_text(size= txt_sz-4)) +
    
    
    theme(plot.title = element_text(),
          text = element_text(size= txt_sz),
          panel.spacing = unit(2, "lines"),
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'black'),
          axis.ticks.x = element_line(),
          axis.line.y = element_line(),
          
    ) +
    scale_y_continuous(expand=c(0,0), limits = c(0,800), 
                       breaks = seq(0, 800, by = 200),labels=comma) +
    
    scale_x_continuous(expand=c(0,0)) +
    guides(fill = guide_legend(nrow = 2)) +
    labs(y =bquote("National GHG Emissions (CO"[2]*"e)")) +
    
    #Add colour
    scale_fill_manual(values = c('Rest of Canada'="#C9C9C9",'Alberta'="#4472C4")) 
  
  p2<-ggplot(E_data) +
    geom_area(aes(x = Year, y = GHGs, fill = Zone), colour = "black", 
              alpha=0.8, size=0.25) +
    
    # Set the theme for the plot
    theme_minimal() +
    theme(panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA)) +
    
    # Text and legend
    theme(axis.text.x = element_text(vjust = 1,color="black"),
          axis.text.y = element_text(color="black"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = 'none',
          plot.caption = element_text(size=txt_sz-2,hjust = 1)) +
    
    
    theme(plot.title = element_text(),
          text = element_text(size= txt_sz),
          panel.spacing = unit(2, "lines"),
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'black'),
          axis.ticks.x = element_line(),
          axis.line.y = element_line(),
          
    ) +
    scale_y_continuous(expand=c(0,0), limits = c(0,120), 
                       breaks = seq(0, 200, by = 30),labels=comma) +
    
    scale_x_continuous(expand=c(0,0)) +
    
    #Add colour
    scale_fill_manual(values = c('Rest of Canada'="#C9C9C9",'Alberta'="#4472C4")) 
  
  # Grab legend
  legend <- get_legend(p1) 
  p1 <- p1 + theme(legend.position ="none")
  
  xsubtitle = textGrob(
    "Scales vary, data from Environment and Climate Change Canada ",
    gp = gpar(fontface = 3, fontsize = 10),
    hjust = 1,
    x = 1
  )
  
  grid.arrange(plot_grid(plot_grid(p1,p2,nrow=1, align="h",axis='l',rel_widths = c(1,1)),legend,rel_widths = c(1,0.2)),
               ncol=1,nrow=2,
               xsubtitle,
               heights=c(1,0.05))
  
}

################################################################################  
## FUNCTION: emissions_type_prov
## Plot stacked area of Alberta emissions vs Canada emissions for electricity and total. Not same axis
##
## INPUTS: 
##    GHG_AB_CAN_Elec - Filtered data
################################################################################
emissions_type_prov <- function(GHG_allprovs) {
  
 # GET TERRITORY EMISSIONS
  # Determine which emissions come from the territories
  GHG_provs<-GHG_allprovs %>%
    filter(!Zone == c('Canada')) %>%
    group_by(Year,Type)%>%
    summarise(GHGs_provs=sum(GHGs)) %>%
    ungroup()
  
  # Filter all 
  GHG_Can<-GHG_allprovs %>%
    filter(Zone =='Canada')%>%
    rename("GHGs_can"=GHGs) %>%
    select(.,'Year','GHGs_can','Type')
  
  GHG_Terr<-merge(GHG_provs,GHG_Can) %>%
    mutate("GHGs_terr"=GHGs_can-GHGs_provs,
           "Zone"="Territories")%>%
    select(.,'Year','GHGs_terr','Type')
    
  # GROUP OTHER SMALL EMITTERS IN WITH TERRITORIES
    to_remove_a<-c('Canada','Manitoba','Newfoundland and Labrador','Prince Edward Island','New Brunswick')
    to_remove_b<-c('Manitoba','Newfoundland and Labrador','Prince Edward Island','New Brunswick')
    prov_keep<-c("Rest of Canada",'Quebec','British Columbia','Ontario','Nova Scotia','Saskatchewan','Alberta')
    prov_cols<-c("Rest of Canada"='#252323',
                 'Quebec'='#525252',
                 'British Columbia'='#767171',
                 'Ontario'='#A6A6A6',
                 'Nova Scotia'='#C9C9C9',
                 'Saskatchewan'="#EDEDED",
                 'Alberta'="#4472C4")
    
    
    # Group other small provinces with territories
    GHG_Small<-GHG_allprovs %>%
      filter(Zone %in% to_remove_b)%>%
      group_by(Year,Type)%>%
      summarise(GHGs_small=sum(GHGs)) 
    
    GHG_Rest<-merge(GHG_Terr,GHG_Small) %>%
      mutate("GHGs"=GHGs_small+GHGs_terr,
             "Zone"="Rest of Canada")%>%
      select(.,'Year','GHGs','Type','Zone')
  
  # Add territories to small total group and rename
  data<-rbind(GHG_allprovs,GHG_Rest) %>%
    filter(!Zone %in% to_remove_a) 
  data$Zone <- factor(data$Zone, levels=prov_keep)
  
  # Set figure text size
  txt_sz=16
  
  # Create two new datasets
  All_data<-data %>%
    filter(Type=='Total')
  E_data<-data %>%
    filter(Type=='Electricity')
  
  
  # Plot for total emissions
  p1<-ggplot(All_data) +
    geom_area(aes(x = Year, y = GHGs, fill = Zone), colour = "black", 
              alpha=0.8, size=0.25) +
    
    # Set the theme for the plot
    theme_minimal() +
    theme(panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA)) +
    
    # Text and legend
    theme(axis.text.x = element_text(vjust = 1,color="black"),
          axis.text.y = element_text(color="black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size= txt_sz+4, face = "bold",family=Plot_Text_bf),
          plot.title = element_text(size= txt_sz-2,hjust=0.5),
          legend.title=element_blank(),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent',colour ='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          legend.position = "bottom",
          legend.text=element_text(size= txt_sz-4)) +
    
    
    theme(plot.title = element_text(family=Plot_Text),
          text = element_text(size= txt_sz),
          panel.spacing = unit(2, "lines"),
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'black'),
          axis.ticks.x = element_line(),
          axis.line.y = element_line(),
          axis.line.x = element_line(),
    ) +
    scale_y_continuous(expand=c(0,0), limits = c(0,800), 
                       breaks = seq(0, 800, by = 160),labels=comma) +
    
    scale_x_continuous(expand=c(0,0)) +
    guides(fill = guide_legend(ncol = 1)) +
    labs(title="Total Emissions",y =bquote("National GHG Emissions (Mt CO"[2]*"e)")) +
    
    #Add colour
    scale_fill_manual(values = prov_cols) 
  
  # Plot for electricity emissions
  p2<-ggplot(E_data) +
    geom_area(aes(x = Year, y = GHGs, fill = Zone), colour = "black", 
              alpha=0.8, size=0.25) +
    
    # Set the theme for the plot
    theme_minimal() +
    theme(panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA)) +
    
    # Text and legend
    theme(axis.text.x = element_text(vjust = 1,color="black"),
          axis.text.y = element_text(color="black"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size= txt_sz-2,hjust=0.5),
          legend.position = 'none') +
    
    
    theme(plot.title = element_text(),
          text = element_text(size= txt_sz),
          panel.spacing = unit(2, "lines"),
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'black'),
          axis.ticks.x = element_line(),
          axis.line.y = element_line(),
          axis.line.x = element_line(),
          
    ) +
    scale_y_continuous(expand=c(0,0), limits = c(0,125), 
                       breaks = seq(0, 125, by = 25),labels=comma) +
    
    scale_x_continuous(expand=c(0,0)) +
    
    labs(title="Electricity Sector Emissions") +
    #Add colour
    scale_fill_manual(values = prov_cols) 
  
  # Grab legend
  legend <- get_legend(p1) 
  p1 <- p1 + theme(legend.position ="none")
  
  xsubtitle = textGrob(
    "Scales vary, data from Environment and Climate Change Canada ",
    gp = gpar(fontface = 3, fontsize = 10),
    hjust = 1,
    x = 1
  )
  
  # Plot result (combine graphs)
  grid.arrange(plot_grid(plot_grid(p1,p2,nrow=1, align="h",axis='l',rel_widths = c(1,1)),legend,rel_widths = c(1,0.2)),
               ncol=1,nrow=2,
               xsubtitle,
               heights=c(1,0.05))
  
}

################################################################################  
## FUNCTION: elec_emissions_prov
## Plot stacked area of Alberta emissions vs Canada emissions for electricity.
##
## INPUTS: 
##    GHG_AB_CAN_Elec - Filtered data
################################################################################
elec_emissions_prov <- function(GHG_allprovs) {
  
  # GET TERRITORY EMISSIONS
  # Determine which emissions come from the territories
  GHG_provs<-GHG_allprovs %>%
    filter(!Zone == c('Canada')) %>%
    group_by(Year,Type)%>%
    summarise(GHGs_provs=sum(GHGs)) %>%
    ungroup()
  
  # Filter all 
  GHG_Can<-GHG_allprovs %>%
    filter(Zone =='Canada')%>%
    rename("GHGs_can"=GHGs) %>%
    select(.,'Year','GHGs_can','Type')
  
  GHG_Terr<-merge(GHG_provs,GHG_Can) %>%
    mutate("GHGs_terr"=GHGs_can-GHGs_provs,
           "Zone"="Territories")%>%
    select(.,'Year','GHGs_terr','Type')
  
  # GROUP OTHER SMALL EMITTERS IN WITH TERRITORIES
  to_remove_a<-c('Canada','Manitoba','Newfoundland and Labrador','Prince Edward Island','Quebec','British Columbia')
  to_remove_b<-c('Manitoba','Newfoundland and Labrador','Prince Edward Island','Quebec','British Columbia')
  prov_keep<-c("Rest of Canada",'New Brunswick','Ontario','Nova Scotia','Saskatchewan','Alberta')
  prov_cols<-c("Rest of Canada"='#252323',
               'New Brunswick'='#626262',
               'Ontario'='#A6A6A6',
               'Nova Scotia'='#C9C9C9',
               'Saskatchewan'="#EDEDED",
               'Alberta'="#4472C4")
  
  
  # Group other small provinces with territories
  GHG_Small<-GHG_allprovs %>%
    filter(Zone %in% to_remove_b)%>%
    group_by(Year,Type)%>%
    summarise(GHGs_small=sum(GHGs)) 
  
  GHG_Rest<-merge(GHG_Terr,GHG_Small) %>%
    mutate("GHGs"=GHGs_small+GHGs_terr,
           "Zone"="Rest of Canada")%>%
    select(.,'Year','GHGs','Type','Zone')
  
  # Add territories to small total group and rename
  data<-rbind(GHG_allprovs,GHG_Rest) %>%
    filter(!Zone %in% to_remove_a) 
  data$Zone <- factor(data$Zone, levels=prov_keep)
  
  # Set figure text size
  txt_sz=14
  
  # Create two new datasets
  All_data<-data %>%
    filter(Type=='Total')
  E_data<-data %>%
    filter(Type=='Electricity')
  
  # Plot for electricity emissions
  ggplot(E_data) +
    geom_area(aes(x = Year, y = GHGs, fill = Zone), colour = "black", 
              alpha=1, size=0.25) +
    
    # Set the theme for the plot
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA)) +
    
    # Text and legend
    theme(text = element_text(size= txt_sz),
          axis.text.x = element_text(vjust = 1,color="black"),
          axis.text.y = element_text(color="black"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size= txt_sz+2),
          plot.title = element_text(size= txt_sz-4,hjust=0.5),
          plot.caption = element_text(hjust = 1,size = txt_sz-4,face = "italic"),             # Plot subtitle size (if present)
          legend.title=element_blank(),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.size = unit(0.5, "cm"),
          legend.background = element_rect(fill='transparent',colour ='transparent'),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
          legend.position = c(0.84, 0.92),
          legend.text=element_text(size= txt_sz-4)) +
    
    
    theme(plot.title = element_text(),
          panel.spacing = unit(2, "lines"),
          #panel.grid.major.y = element_line(size=0.25,linetype=1,color = 'black'),
          axis.ticks.x = element_line(),
          axis.line.y = element_line(),
          axis.line.x = element_line(),
          
    ) +
    scale_y_continuous(expand=c(0,0), limits = c(0,125), 
                       breaks = seq(0, 125, by = 25),labels=comma) +
    
    scale_x_continuous(expand=c(0,0)) +
    
    labs(y =bquote("Electricity Sector Emissions (Mt CO"[2]*"e)"),caption="Data from Canada's Official Greenhouse Gas Inventory") +
    #Add colour
    scale_fill_manual(values = prov_cols) 
  
}

################################################################################  
## FUNCTION: emissions_type_prov
## Plot stacked area of Alberta emissions vs Canada emissions for electricity and total. Not same axis
##
## INPUTS: 
##    GHG_AB_CAN_Elec - Filtered data
################################################################################
emissions_type_prov2 <- function(GHG_allprovs) {
  
  # GET TERRITORY EMISSIONS
  # Determine which emissions come from the territories
  GHG_provs<-GHG_allprovs %>%
    filter(!Zone == c('Canada')) %>%
    group_by(Year,Type)%>%
    reframe(GHGs_provs=sum(GHGs)) 
  
  # Filter all 
  GHG_Can<-GHG_allprovs %>%
    filter(Zone =='Canada')%>%
    rename("GHGs_can"=GHGs) %>%
    select(.,'Year','GHGs_can','Type')
  
  GHG_Terr<-merge(GHG_provs,GHG_Can) %>%
    mutate("GHGs_terr"=GHGs_can-GHGs_provs,
           "Zone"="Territories")%>%
    select(.,'Year','GHGs_terr','Type')
  
  # GROUP OTHER SMALL EMITTERS IN WITH TERRITORIES
  to_remove_a<-c('Canada','Manitoba','Newfoundland and Labrador','Prince Edward Island','New Brunswick')
  to_remove_b<-c('Manitoba','Newfoundland and Labrador','Prince Edward Island','New Brunswick')
  prov_keep<-c("Rest of Canada",'Quebec','British Columbia','Ontario','Nova Scotia','Saskatchewan','Alberta')
  prov_cols<-c("Rest of Canada"='#252323',
               'Quebec'='#525252',
               'British Columbia'='#767171',
               'Ontario'='#A6A6A6',
               'Nova Scotia'='#C9C9C9',
               'Saskatchewan'="#EDEDED",
               'Alberta'="#4472C4")
  
  # Group other small provinces with territories
  GHG_Small <- GHG_allprovs %>%
    filter(Zone %in% to_remove_b) %>%
    group_by(Year, Type) %>%
    reframe(GHGs_small = sum(GHGs))
  
  GHG_Rest <- merge(GHG_Terr, GHG_Small, by = c("Year", "Type")) %>%
    mutate(GHGs = GHGs_small + GHGs_terr, Zone = "Rest of Canada") %>%
    select(Year, GHGs, Type, Zone)
  
  # Add territories to small total group and rename
  data <- rbind(GHG_allprovs, GHG_Rest) %>%
    filter(!Zone %in% to_remove_a)
  data$Zone <- factor(data$Zone, levels = prov_keep)
  
  data$Type <- factor(data$Type, levels=c('Total','Electricity'))
  
  # Facet breaks
  my_breaks <- function(x) { if (max(x) < 400) seq(0, 125, 25) else seq(0, 800, 160) }
  my_limits <- function(x) { if (max(x) < 200) c(0, 125) else c(0, 800) }
  
  # Plot for total emissions
  ggplot(data) +
    geom_area(aes(x = Year, y = GHGs, fill = Zone), colour = "black", 
              alpha=0.8, size=0.25) +
    theme_bw() +
    facet_wrap(~Type, strip.position = "top",scales="free_y",
               axes = "all", axis.labels = "all") +
  
    # Set the theme for the plot
    theme(text=element_text(family=Plot_Text)) +
    theme(axis.text = element_text(color="black"),
          axis.title = element_text(size = GenText_Sz+6,family=Plot_Text_bf),
          axis.text.x = element_text(angle = 0, hjust=1,vjust=0.5,color="black",size = GenText_Sz-6),
          plot.title = element_blank(),
          text = element_text(size=GenText_Sz),
          axis.title.x=element_blank(),
          legend.text = element_text(size = GenText_Sz-6),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "right",
          #panel.grid.major.y = element_line(size=0.25,linetype=2,color = 'gray70'),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.caption = element_text(size = GenText_Sz-12,family = Plot_Text_it),
          
          strip.placement = "outside",
          strip.text = element_text(size = GenText_Sz-6, color = "black"),
          strip.background = element_rect(colour=NA, fill=NA),
          #panel.spacing = unit(0,'lines'),
          
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.background = element_rect(fill='transparent'),
          legend.key.size = unit(0.3, "cm"),
          legend.box.background = element_rect(fill='transparent', colour = "transparent"),
    ) +
    
    scale_y_continuous(expand=expansion(c(0,0)), limits = my_limits, 
                       breaks=my_breaks) +
    
    scale_x_continuous(expand=c(0,0)) +
    labs(title="Total Emissions",y =expression("National GHG Emissions (Mt CO"[2]*"e)"),caption="Data from Canada's Official Greenhouse Gas Inventory, scales vary") +
    
    #Add colour
    scale_fill_manual(values = prov_cols) +
    guides(fill = guide_legend(ncol = 1)) 
    
  
}
