#Pour choisir les couleurs
  #https://www.w3schools.com/colors/colors_rgb.asp

#Trouver un site pour des palettes de couleurs 

#Important: pour voir les changements qu'on apporte à un thème, il faut arrêter
 #la session R, la repartir et ensuite rerouler le shiny

library(dashboardthemes)

clessnTheme <- shinyDashboardThemeDIY(
  
  # general ####
  appFontFamily = "Superpose",
    #Je n'ai pas vu de différence en passant
    #d'Arial à Calibri dans le header
  
  #Couleur du texte standard dans le shiny
  appFontColor = "rgb(247, 245, 245)" # Blanc cassé/gris
    #Pas la couleur du texte dans le header
  
  #Couleur de fond du body
  ,bodyBackColor = "rgb(24,24,24)" # Presque noir
  
  # header ####
  
  #Couleur du rectangle derrière le texte
  ,logoBackColor = "rgb(24,24,24)" # Presque noir
  
  #Bouton qui ouvre le sidebar
  ,headerButtonIconColor = "rgb(254, 236, 32)" # Jaune
  ,headerButtonBackColor = "rgb(64,64,64)" #Gris medium 
  ,headerButtonBackColorHover = "rgb(24,24,24)" # Presque noir
  ,headerButtonIconColorHover = "rgb(254, 236, 32)" # Jaune

  
  #Ligne horizontale
     #Couleur
  ,headerBackColor = "rgb(0,0,0)" #noir
     #Couleur de l'ombre
  ,headerBoxShadowColor = "rgb(211,211,211)" #gris pale
     #Grandeur de l'ombre
  ,headerBoxShadowSize = "1px 1px 1px"
       #je ne comprends pas la différence entre les 3 valeurs
  
  # sidebar ####
  
  #Couleur du fond du sidebar (peut être une couleur ou un dégradé)
  
  # Exemple de dégradé
  # ,sidebarBackColor = cssGradientThreeColors(
  #   direction = "right"
  #   ,colorStart = "rgb(20,97,117)"
  #   ,colorMiddle = "rgb(56,161,187)"
  #   ,colorEnd = "rgb(3,22,56)"
  #   ,colorStartPos = 0
  #   ,colorMiddlePos = 50
  #   ,colorEndPos = 100
  # )
  # Exemple de couleur unie
  ,sidebarBackColor = "rgb(64,64,64)" #Gris medium
    
  #Largeur du sidebar
  ,sidebarPadding = 2
  
  ## sidebarMenu, tabs ####
    #Rectangle autour du texte
  ,sidebarMenuBackColor = "rgb(64,64,64)" #Gris medium
    #Rectangle autour du texte - hovered
  ,sidebarTabBackColorHover = "rgb(254, 236, 32)" # Jaune
    #Rectangle autour du texte - sélectionné
  ,sidebarTabBackColorSelected = "rgb(254, 236, 32)" # Jaune
  
  
    #???
  ,sidebarMenuPadding = 0
    #???
  ,sidebarMenuBorderRadius = 5
  
    #Couleur du text de l'item
  ,sidebarTabTextColor = "rgb(254, 236, 32)" # Jaune
    #Couleur du text de l'item - hovered
  ,sidebarTabTextColorHover = "rgb(64,64,64)" #Gris medium
    #Couleur du text de l'item - sélectionné
  ,sidebarTabTextColorSelected = "rgb(64,64,64)" #Gris medium
    #Grandeur du text de l'item
  ,sidebarTabTextSize = 14
  
    #Style bordure (haut, droit, bas, gauche)
  ,sidebarTabBorderStyle = "none none solid none"
    #Style bordure - hovered
  ,sidebarTabBorderStyleHover = "none none solid none"
      #none, solid, dashed, peut-être d'autres? 
  
    #Couleur bordure
  ,sidebarTabBorderColor = "rgb(211,211,211)" # Gris pale
    #Couleur bordure - hovered
  ,sidebarTabBorderColorHover = "rgb(254, 236, 32)" # Jaune
  
    #Grosseur bordure
  ,sidebarTabBorderWidth = 3
    #Grosseur bordure - hovered
  ,sidebarTabBorderWidthHover = 0
  
    #Bordure coins - hovered
  ,sidebarTabRadiusHover = "20px 20px 20px 20px"
    #Bordure coins - sélectionné
  ,sidebarTabRadiusSelected = "20px 20px 20px 20px"
  
  

  #Ombre du sidebar
    #Grandeur de l'ombre
  ,sidebarShadowRadius = "3px 3px 3px"
    #Couleur de l'ombre
  ,sidebarShadowColor = "rgb(211,211,211)" # Gris pale

  
  #???  
  ,sidebarUserTextColor = "rgb(24,24,24)" # Presque noir
    #J'imagine que ça permet de contrôler la couleur du texte que l'utilisateur
    #rentre. Par contre ça n'a pas rapport à des textInput ni aux inputs
    #de SearchForm donc je sais pas encore son utilité

  #Input sidebarSearchForm
    #Couleur de fond
  ,sidebarSearchBackColor = "rgb(255, 255, 170)" # Jaune plus pâle
    #Couleur de la loupe (ou de l'icone utilisée)
  ,sidebarSearchIconColor = "rgb(24,24,24)" # Presque noir
    #Couleur de la bordure du input
  ,sidebarSearchBorderColor = "rgb(24,24,24)" # Presque noir
  
  
  # boxes ####
  
  #Couleur de fond des box et infoBox mais pas des valueBox
    #contrôle la couleur de fond de la partie de l'information. 
  ,boxBackColor = "rgb(64,64,64)" #Gris medium
  #grandeur de la partie ronde des box 
  ,boxBorderRadius = 0
  #ombre
  ,boxShadowSize = "3px 3px 3px"
  ,boxShadowColor = "rgb(211,211,211)" # Gris pale
  
  #Pour changer la grosseur du titre dans les box SEULEMENT
  ,boxTitleSize = 30
  #??
  ,boxDefaultColor = "rgb(64,64,64)" #Gris medium
  
  #À documenter: lorsqu'on fait une "box", on peut lui donner une couleur parmi:
    #primary, info, success, warning et danger. Elles sont déjà prédéfinies mais
    #les arguments suivants nous permettent de les contrôler (fond et couleur texte)
  #fond
  ,boxPrimaryColor = "rgb(64,64,64)" #Gris medium
  ,boxInfoColor = "rgb(254, 236, 32)" # Jaune
  ,boxSuccessColor = "rgb(247, 245, 245)" # Blanc cassé/gris
  ,boxWarningColor = "rgb(254, 236, 32)" # Jaune
  ,boxDangerColor = "rgb(254, 236, 32)" # Jaune 
  #  "rgb(255, 255, 170)" # Jaune plus pâle

  #texte
  ,primaryFontColor = "rgb(247, 245, 245)" # Blanc cassé/gris
  ,infoFontColor = "rgb(24,24,24)" # Presque noir
  ,successFontColor = "rgb(24,24,24)" # Presque noir
  ,warningFontColor = "rgb(24,24,24)" # Presque noir
  ,dangerFontColor = "rgb(24,24,24)" # Presque noir
  
  ## tab boxes ####
  
  #couleur de l'onglet sélectionné
  ,tabBoxTabColor = "rgb(64,64,64)" #Gris medium
  #grandeur du titre des onglets
  ,tabBoxTabTextSize = 12
  #couleur du titre des onglets
  ,tabBoxTabTextColor = "rgb(24,24,24)" # Presque noir
  #couleur du titre des onglets - sélectionné
  ,tabBoxTabTextColorSelected = "rgb(254, 236, 32)" # Jaune
  #couleur du fond de la box (partie contenu)  
  ,tabBoxBackColor = "rgb(211,211,211)" # Gris pale
  #couleur du contour
  ,tabBoxHighlightColor = "rgb(211,211,211)" # Gris pale
  #coins arrondis
  ,tabBoxBorderRadius = 0
  
  # inputs ####
  ,buttonBackColor = "rgb(24,24,24)" # Presque noir
  ,buttonTextColor = "rgb(247, 245, 245)" # Blanc cassé/gris
  ,buttonBorderColor = "rgb(24,24,24)" # Presque noir
  ,buttonBorderRadius = 0
  
  ,buttonBackColorHover = "rgb(254, 236, 32)" # Jaune
  ,buttonTextColorHover = "rgb(24,24,24)" # Presque noir
  ,buttonBorderColorHover = "rgb(24,24,24)" # Presque noir
  
  ,textboxBackColor = "rgb(24,24,24)" # Presque noir
  ,textboxBorderColor = "rgb(24,24,24)" # Presque noir
  ,textboxBorderRadius = 0
  ,textboxBackColorSelect = "rgb(24,24,24)" # Presque noir
  ,textboxBorderColorSelect = "rgb(24,24,24)" # Presque noir
  
  # tables ####
  ,tableBackColor = "rgb(64,64,64)" #Gris medium
  ,tableBorderColor = "rgb(247, 245, 245)" # Blanc cassé/gris
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

