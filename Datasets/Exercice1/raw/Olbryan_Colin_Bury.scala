import scala.io.StdIn._
import scala.util.Random
object Main {
  def main(args: Array[String]): Unit = {

   /*initialisation des variables*/
/*pour enlever les espaces dans les inputs*/
    var espace = ""



    /*prix boisson, sucre , lait et final2 en string pour que cela print 0.10 et non 0.1*/
    var prix_boisson = ""
    var prix_sucre = ""
    var prix_lait = ""
    var prix_final= 0.0
    var prix_final2 = ""


    /*quantitée nécessaire*/
    var qt_sucre = 0
    var qt_cafe = 0
    var qt_lait = 0.0 //en L
    /*choix*/
    var mode = ""
    var nom_boisson = "" /*pour les messages*/
    var boisson=""
    var choix_sucre=""
    var choix_lait = ""
    var dose_lait = ""
    var choix_taille = ""
    /*stocks*/
    var stock_cafe = 5000
    var stock_sucre = 3000
    var stock_lait = 50.0 /*en L*/
    var choix_stock = "" //pour le mode admin




var fin = false
var Client = true
while(!fin){
  while(mode != "1" && mode != "2" && mode != "3"){
    espace=""
    println("    Nospresso Café" + "\nVeuillez sélectionner votre mode :" + "\n1) Client" + "\n2) Admin" + "\n3) Quitter")
    /*choix mode*/
    mode = readLine(">")
    for(i<-mode){
      if(i!=' '){/*pour chaque caractère de 'mode', on ajoute à 'espace' le caractère, s'il n'y a pas d'espace*/
        espace += i
      }
    }
    mode = espace /*mode devient espace => il n'y a plus d'espace*/
    if (mode != "1" && mode != "2" && mode != "3")  {
      println(" Veuillez sélectionner le bon mode comme indiqué : 1,2 ou 3" + "\n")
      Thread.sleep(1000)
    }
  }

/*suite après choix du mode*/

    if (mode=="1"){
      while(Client){
      val code_paiement = Random.alphanumeric.take(5).mkString
      /*vérification de l'input même si l'utilisateur utilise un caractère spécial qui fait crasher le code*/
      while (boisson != "1" && boisson != "2" && boisson != "3"){
        println("Veuillez sélectionner votre boisson :" + "\n1) Expresso - CHF 2.00" + "\n2) Cappuccino - CHF 2.50" + "\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")

        /*choix mode*/
        boisson = readLine(">")
        /*réinitialisation de espace*/
        espace = ""
        /* sans espace*/
        for(i<-boisson){
          if(i!=' '){
            espace += i
          }
        }
        boisson = espace
        if (boisson!= "1" && boisson != "2" && boisson!= "3")  {
          println(" Veuillez sélectionner la bonne boisson comme indiqué : 1,2 ou 3" + "\n")
          Thread.sleep(1000)
        }
      }
      if(boisson=="1"){
        /*données boisson*/
        nom_boisson = "Expresso"
        prix_boisson = "CHF 2.00"
        prix_final+= 200
        qt_cafe = 800
        while(choix_sucre != "1" && choix_sucre != "2" && choix_sucre != "3" && choix_sucre !="4"){
          println("Souhaitez-vous ajouter du sucre ? " + "\n1) Sans sucre" + "\n2) Peu (5g) - CHF 0.10" + "\n3) Moyen (10g) - CHF 0.20" + "\n4) Beaucoup (15g) - CHF 0.30")
          /*choix sucre*/
          choix_sucre = readLine(">")
          /*sans espace*/
          espace = ""
          for(i<-choix_sucre){
            if(i!=' '){
              espace += i
            }
          }
          choix_sucre = espace
          if (choix_sucre != "1" && choix_sucre != "2" && choix_sucre != "3" && choix_sucre != "4"){
            println(" Veuillez sélectionner la bonne quantité comme indiqué : 1,2, 3 ou 4" + "\n")
            Thread.sleep(1000)
          }
        }/*affichage*/
        println("Boisson sélectionnée : " + nom_boisson)
        /*montrer qt sucre + ajustement prix*/
        if (choix_sucre=="1"){
          qt_sucre=0
          println("Niveau de sucre : sans sucre")

        }
        else if (choix_sucre=="2"){
          qt_sucre = 500
          println("Niveau de sucre : Peu (5g)")
          prix_sucre = "CHF 0.10"
          prix_final +=  10
        }
        else if (choix_sucre=="3"){
          qt_sucre = 1000
          println("Niveau de sucre : Moyen (10g)")
          prix_sucre = "CHF 0.20"
          prix_final +=  20
        }
        else {
          qt_sucre = 1500
          println("Niveau de sucre : Beaucoup (15g)")
          prix_sucre = "CHF 0.30"
          prix_final+= 30
        }
        prix_final2 = "CHF " + (prix_final/100).toString
        /*vérification stock + paiement*/
        if(stock_cafe>=qt_cafe){
          if(stock_sucre>= qt_sucre){
            if(choix_sucre=="1"){
              println("Prix total : " + prix_boisson  + " = " + prix_final2 + "0" + "\n")//+"0" pour avoir 2.00 et pas 2.0
            }
            else {
              println("Prix total : " + prix_boisson + " + " + prix_sucre + " = " + prix_final2 + "0" +  "\n")
            }
            stock_cafe -= qt_cafe
            stock_sucre -= qt_sucre
            Thread.sleep(2000)
            println("Veuillez payer en utilisant Twint." + "\nVotre code de paiement est : " + code_paiement + "\n(En attente de validation du paiement...)" + "\n")
            Thread.sleep(3000)
            println("Merci ! Votre paiement a été accepté. " + "\n")
            Thread.sleep(2000)
            println("Préparation de votre boisson... " + "\n[...]")
            Thread.sleep(3000)
            println("Votre " + nom_boisson + " est prêt ! Bonne dégustation ! " + "\n")
            Client = false
            Thread.sleep(4000)



          }
          else{

            println("Erreur : Quantité de sucre insuffisante pour préparer " + "\nla boisson sélectionnée." + "\nVeuillez choisir une autre boisson ou vérifier les stocks" + "\nstocks en mode Admin." + "\n")
            Thread.sleep(3000)
          }
        }
        else{

          println("Erreur : Quantité de café insuffisante pour préparer " + "\nla boisson sélectionnée." + "\nVeuillez choisir une autre boisson ou vérifier les" + "\nstocks en mode Admin." +"\n")
          Thread.sleep(3000)
        }
      }

      else if (boisson=="2" || boisson=="3"){
        /*qt pour un cappuccino + nom*/
        if (boisson=="2"){
          nom_boisson = "Cappuccino"
          prix_boisson = "CHF 2.50"
          qt_cafe = 600
          qt_lait = 10
          prix_final+=250
        }

        else{/*choix de la taille du latte + nom*/
          while(choix_taille != "1" && choix_taille!="2" && choix_taille != "3"){
            println("Veuillez sélectionner la taille du Latte : " + "\n1) Petit" + "\n2) Moyen" + "\n3) Grand")
            choix_taille = readLine(">")
            /*sans espace*/
            espace = ""
            for(i<-choix_taille){
              if(i!=' '){
                espace += i
              }
            }
            choix_taille = espace
            if(choix_taille != "1" && choix_taille!="2" && choix_taille != "3"){
              println("Veuillez sélectionner la bonne taille comme indiqué : 1, 2, ou 3")
              Thread.sleep(1000)
            }
          }
          /*qt selon la taille du latte + prix*/
          if (choix_taille == "1"){
            prix_boisson = "CHF 2.70"
            qt_cafe = 600
            qt_lait = 12
            nom_boisson = "Latte (Petit)"
            prix_final+=270
          }
          else if (choix_taille =="2"){
            prix_boisson = "CHF 3.20"
            qt_cafe = 800
            qt_lait = 15
            nom_boisson = "Latte (Moyen)"
            prix_final+=320
          }
          else {
            prix_boisson = "CHF 3.70"
            qt_cafe = 1200
            qt_lait = 20
            nom_boisson = "Latte (Grand)"
            prix_final+=370
          }

        }
        prix_final2= "CHF " + (prix_final/100).toString


        while(choix_sucre != "1" && choix_sucre != "2" && choix_sucre != "3" && choix_sucre !="4"){
          println("Souhaitez-vous ajouter du sucre ? " + "\n1) Sans sucre" + "\n2) Peu (5g) - CHF 0.10" + "\n3) Moyen (10g) - CHF 0.20" + "\n4) Beaucoup (15g) - CHF 0.30")
          /*choix sucre*/
          choix_sucre = readLine(">")
          espace = ""
          for(i<-choix_sucre){
            if(i!=' '){
              espace += i
            }
          }
          choix_sucre = espace
          if (choix_sucre != "1" && choix_sucre != "2" && choix_sucre != "3" && choix_sucre !="4"){
            println(" Veuillez sélectionner la bonne quantité comme indiqué : 1,2, 3 ou 4" + "\n")
            Thread.sleep(1000)
          }
        }
        while(choix_lait != "1" && choix_lait != "2"){
          println("Souhaitez-vous ajouter du lait en supplément ?" + "\n(disponible que pour Cappucino ou Latte)" + "\n1) Oui " + "\n2) Non " )

          /*choix lait*/
          choix_lait = readLine(">")
          espace = ""
          for(i<-choix_lait){
            if(i!=' '){
              espace += i
            }
          }
          choix_lait= espace
          if(choix_lait != "1" && choix_lait != "2"){
            println(" Veuillez sélectionner Oui ou Non, comme indiqué : 1 ou 2" + "\n")
          }
        }
        /*si oui*/
        if (choix_lait == "1"){
          while(dose_lait!="1" && dose_lait!="2" && dose_lait!= "3"){
            println("Combien de dose ? ")
            /*choix dose*/
            dose_lait = readLine(">")
            espace = ""
            for(i<-dose_lait){
              if(i!=' '){
                espace += i
              }
            }
            dose_lait= espace
            if(dose_lait != "1" && dose_lait != "2" && dose_lait!="3"){
              println(" Veuillez sélectionner 1 à 3 dose de 50mL de lait, comme indiqué : 1, 2 ou 3" + "\n")
              Thread.sleep(1000)
            }
          }}
        /*affichage*/
        println("Boisson sélectionnée : " + nom_boisson)
        /*montrer qt sucre*/
        if (choix_sucre=="1"){
          qt_sucre = 0
          println("Niveau de sucre : sans sucre")


        }
        else if (choix_sucre=="2"){
          qt_sucre = 500
          println("Niveau de sucre : Peu (5g)")
          prix_sucre = "CHF 0.10"
          prix_final+=10

        }
        else if (choix_sucre=="3"){
          qt_sucre = 1000
          println("Niveau de sucre : Moyen (10g)")
          prix_sucre = "CHF 0.20"
          prix_final+=20

        }
        else {
          qt_sucre = 1500
          println("Niveau de sucre : Beaucoup (15g)")
          prix_sucre = "CHF 0.30"
          prix_final+=30

        }
        prix_final2= "CHF " + (prix_final/100).toString
        /*affichage lait*/
        if (choix_lait == "2"){
          println("Lait en supplément : Non")
        }
        else{

          if (dose_lait=="1"){
            println("Lait en supplément : 50 mL")
            prix_lait = "CHF 0.05"
            prix_final+=5
            qt_lait+=5
          }
          else if(dose_lait=="2"){
            println("Lait en supplément : 100mL")
            prix_lait = "CHF 0.10"
            prix_final+=10
            qt_lait+=10
          }
          else{
            println("Lait en supplément : 150mL")
            prix_lait = "CHF 0.15"
            prix_final+=15
            qt_lait+=15
          }
        }


        /*vérification des stocks et paiement*/
        prix_final2= "CHF " + (prix_final/100).toString
        /*ajoute le 0 manquant, quand il y a 2.5 ou 2.0 par ex.*/
        if(((prix_final2.contains(".")&& !prix_final2.contains(".0"))|| prix_final2.contains(".0"))&& (!prix_final2.contains(".05") && !prix_final2.contains(".15") && !prix_final2.contains(".25") && !prix_final2.contains(".35") && !prix_final2.contains(".45") && !prix_final2.contains(".55") && !prix_final2.contains(".65") && !prix_final2.contains(".75") && !prix_final2.contains(".85") && !prix_final2.contains(".95"))){
          prix_final2+= "0"
        }
        else{
        }

        if(stock_cafe>=qt_cafe){
          if(stock_sucre>= qt_sucre){
            if(choix_lait=="1"){/*vérification stock si dose supp.*/
              if(stock_lait>=qt_lait){
                if(choix_sucre=="1"){
                  println("Prix total : " + prix_boisson + " + " + prix_lait + " = " + prix_final2 + "\n")
                }
                else{
                  println("Prix total : " + prix_boisson + " + "  + prix_sucre + " + " + prix_lait + " = " + prix_final2 + "\n")
                }
                stock_cafe -= qt_cafe
                stock_sucre -= qt_sucre
                stock_lait-= qt_lait
                Thread.sleep(2000)
                println("Veuillez payer en utilisant Twint." + "\nVotre code de paiement est : " + code_paiement + "\n(En attente de validation du paiement...)" + "\n")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a été accepté. " + "\n")
                Thread.sleep(2000)
                println("Préparation de votre boisson... " + "\n[...]")
                Thread.sleep(3000)
                println("Votre " + nom_boisson + " est prêt ! Bonne dégustation ! " + "\n")
                Client = false
                Thread.sleep(4000)


              }
              else {
                if(boisson=="2"){//pour le cappuccino

                  println("Erreur : Quantité de lait insuffisante pour préparer " + "\nla boisson sélectionnée." + "\nVeuillez choisir une autre boisson ou vérifier les" + "\nstocks en mode Admin." +"\n")
                  Thread.sleep(3000)
                }
                else{//pour le latte

                  println("Erreur : Quantité de lait insuffisante pour préparer "+ "\nla boisson sélectionnée."+ "\nVeuillez choisir une taille plus petite ou essayer" + "\nune autre boisson." + "\n")
                  Thread.sleep(3000)
                }

              }
            }
            else{
              if(choix_sucre=="1"){/*si sucre ou non*/
                println("Prix total : " + prix_boisson + " = " + prix_final2 + "\n")
              }
              else{
                println("Prix total : " + prix_boisson + " + " + prix_sucre + " = " + prix_final2 + "\n")
              }
              if(stock_lait>=qt_lait){
                stock_cafe -= qt_cafe
                stock_sucre -= qt_sucre
                stock_lait-= qt_lait
                Thread.sleep(2000)
                println("Veuillez payer en utilisant Twint." + "\nVotre code de paiement est : " + code_paiement + "\n(En attente de validation du paiement...)" + "\n")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a été accepté. " + "\n")
                Thread.sleep(2000)
                println("Préparation de votre boisson... " + "\n[...]")
                Thread.sleep(3000)
                println("Votre " + nom_boisson + " est prêt ! Bonne dégustation ! " + "\n")
                Client = false
                Thread.sleep(4000)

              }
              else{
                if(boisson=="2"){//pour le cappuccino

                  println("Erreur : Quantité de lait insuffisante pour préparer " + "\nla boisson sélectionnée." + "\nVeuillez choisir une autre boisson ou vérifier les" + "\nstocks en mode Admin." +"\n")
                  Thread.sleep(3000)
                }
                else{//pour le latte

                  println("Erreur : Quantité de lait insuffisante pour préparer "+ "\nla boisson sélectionnée."+ "\nVeuillez choisir une taille plus petite ou essayer" + "\nune autre boisson." + "\n")
                  Thread.sleep(3000)
                }
              }
            }
          }
          //le sucre ne dépend pas de la taille
          else {

            println("Erreur : Quantité de sucre insuffisante pour préparer " + "\nla boisson sélectionnée." + "\nVeuillez choisir une autre boisson ou vérifier les" + "\nstocks en mode Admin." +"\n")
            Thread.sleep(3000)
          }

        }
        else{

          if(boisson=="2"){//pour le cappuccino
            println("Erreur : Quantité de café insuffisante pour préparer " + "\nla boisson sélectionnée." + "\nVeuillez choisir une autre boisson ou vérifier les" + "\nstocks en mode Admin." +"\n")
            Thread.sleep(3000)
          }
          else{//pour le latte
            println("Erreur : Quantité de café insuffisante pour préparer " + "\nla boisson sélectionnée." + "\nVeuillez choisir une taille plus petite ou essayer" + "\nune autre boisson." +"\n")
            Thread.sleep(3000)
          }

        }
      }

        /*réinitialisation des variables choix*/
        boisson = ""
        mode = ""
        choix_lait = ""
        choix_sucre = ""
        dose_lait = ""
        choix_taille = ""
        choix_stock = ""

        /*variables prix*/
        prix_sucre  = ""
        prix_final = 0
        prix_final2=""
        /*var espace*/
        espace =""


        }
    }


  if (mode=="2"){
  var pin = ""
  print("Mode Admin" + "\nEntrez le code PIN : ")
    pin = readLine()
    espace = ""
    for(i<-pin){
      if(i!=' '){
        espace += i
      }
    }
   pin= espace
  if (pin=="434343"){
    println("Accès autorisé." + "\n" + "\nStocks:")
    Thread.sleep(2000)
    println("Poudre de café: " + stock_cafe/100 + "g" + "\nLait          : " + stock_lait/100 + "L" + "\nSucre         : " + stock_sucre/100 + "g")
    Thread.sleep(1000)

    while(choix_stock!="1" && choix_stock!="2"){
      println("Voulez-vous ajouter du stock ?" + "\n1) Oui"  + "\n2) Non")
      choix_stock = readLine(">")
    espace = ""
    for(i<-choix_stock){
      if(i!=' '){
        espace += i
      }
    }
    if(choix_stock!="1" && choix_stock!="2"){
      println("Veuillez sélectionner 1 (Oui) ou 2 (Non)")
      Thread.sleep(1000)
    }
    choix_stock= espace

    }
    if(choix_stock=="1"){
      val ajoutcafe = readLine("Poudre de café: ").toInt
      val ajoutlait = readLine("Lait          : ").toDouble
      val ajoutsucre = readLine("Sucre         : ").toInt
      println("Réapprovisionnement des stocks..." + "\nAjout :")
      Thread.sleep(2000)

      println("Poudre de café: " + ajoutcafe + "\nLait          : " + ajoutlait + "\nSucre         : " + ajoutsucre)
      stock_cafe+= ajoutcafe*100
      stock_lait+=ajoutlait*100
      stock_sucre+=ajoutsucre*100
      println("Niveaux de stock mis à jour.")
      println("Retour au menu principal...")
      Thread.sleep(3000)
    }
    else{
    println("Retour au menu...")
    Thread.sleep(1000)
    }
  }
  else {
    println("Accès refusé")
  }
      }
if(mode=="3"){
  fin = true
      }


  Client = true
  mode = ""
}
    println(" Au revoir !")



  }
}









