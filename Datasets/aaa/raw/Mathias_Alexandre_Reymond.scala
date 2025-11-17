


object Main {
  def main(args: Array[String]): Unit = {
  import scala.io.StdIn.readLine
    import scala.math._
    import scala.util.Random

    var end=false
    var codeAdmin = 434343
    var café=50.0
    var sucre=30.0
    var lait= 500.0
    var caféI= café
    var sucreI= sucre
    var laitI= lait
var choix=0

while(end!=true) {
     choix = readLine("Nospresso Café, Veuillez séléctionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n>").toInt
    if((choix!=1)&&(choix!=2)&&(choix!=3)) {
      do {
      choix=readLine("Nospresso Café, Veuillez séléctionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n>").toInt
      }
      while((choix!=1)&&(choix!=2)&&(choix!=3))
    }
  var caféI= café
  var sucreI= sucre
  var laitI= lait
  var choixC = 0
  var choixC2 = 0
  var choixC3=0
  var choixLatte=0
  var choixC4=0
  var caféIns=false
  var sucreIns=false
  var laitIns=false
  var prixboisson=0.0f
  var prixsucre=0.0f
  var prixlait=0.0f
  var prixTotal=0.0f
  var typeboisson=""
  var typesucre=""
  var typelait=""
  var codeTwint=""
  var end2=false
  var admin1=0
  var admin2=0
  var admin3=0
  var choixAdmin=0
  var choixStock=0
  var choixStock2=0
  var quantitéstock1=0.0
  var quantitéstock2=0.0
  var quantitéstock3=0.0
  var laitcafé=false
  var cafésucre=false
  var laitsucre=false
  var double=false
if (choix==1) {
while(end2!=true) {
  end2=false
  var choixC = 0
  var choixC2 = 0
  var choixC3=0
  var choixLatte=0
  var choixC4=0
  var caféIns=false
  var sucreIns=false
  var laitIns=false
  var prixboisson=0.0f
  var prixsucre=0.0f
  var prixlait=0.0f
  var prixTotal=0.0f
  var typeboisson=""
  var typesucre=""
  var typelait=""
  var codeTwint=""
  var admin1=0
  var admin2=0
  var admin3=0
  var choixAdmin=0
  var choixStock=0
  var choixStock2=0
  var quantitéstock1=0.0
  var quantitéstock2=0.0
  var quantitéstock3=0.0
  var laitcafé=false
  var cafésucre=false
  var laitsucre=false
  var double=false
      choixC =readLine("Veuillez sélectionner votre boisson :" +
        "\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>").toInt
      if((choixC!=1)&&(choixC!=2)&&(choixC!=3)) {
        do {
          choixC =readLine("Veuillez sélectionner votre boisson : " +
            "\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>").toInt
        }
        while((choixC!=1)&&(choixC!=2)&&(choixC!=3))
      }
      if(choixC==3){
        choixLatte=readLine("Quelle taille désirez-vous ?: " +
          "\n1) Petit \n2) Moyen \n3) Grand\n>").toInt
        if((choixLatte!=1)&&(choixLatte!=2)&&(choixLatte!=3)) {
          do {
            choixLatte=readLine("Quelle taille désirez-vous ?: " +
              "\n1) Petit \n2) Moyen \n3) Grand\n>").toInt
          }
          while((choixLatte!=1)&&(choixLatte!=2)&&(choixLatte!=3))
        }
      }
      choixC2= readLine("Souhaitez-vous ajouter du sucre ?: " +
        "\n1) Pas de sucre \n2) Peu de sucre(5g) - CHF 0.10 \n3) Moyen(10g) - CHF 0.20 \n4) Beaucoup(15g) - CHF 0.30\n>").toInt
      if((choixC2!=1)&&(choixC2!=2)&&(choixC2!=3)&&(choixC2!=4)) {
        do {
          choixC2= readLine("Souhaitez-vous ajouter du sucre ?: " +
            "\n1) Pas de sucre " +
            "\n2) Peu de sucre(5g) - CHF 0.10 " +
            "\n3) Moyen(10g) - CHF 0.20 " +
            "\n4) Beaucoup(15g) - CHF 0.30\n>").toInt
        }
        while((choixC2!=1)&&(choixC2!=2)&&(choixC2!=3)&&(choixC2!=4))
      }
      if((choixC==2)||(choixC==3)){
        choixC3=readLine("Voulez-vous ajouter du lait ?: " +
          "\n1) Oui " +
          "\n2) Non\n>").toInt
        if((choixC3!=1)&&(choixC3!=2)) {
          do {
            choixC3=readLine("Voulez-vous ajouter du lait ?: " +
              "\n1) Oui " +
              "\n2) Non\n>").toInt
        }
          while((choixC3!=1)&&(choixC3!=2))
        }
        if(choixC3==1) {
          choixC4= readLine("\n1) Une dose (50ml) - CHF 0.05 " +
            "\n2) Deux doses (100ml) - CHF 0.10  " +
            "\n3) Trois doses (150ml) - CHF 0.15\n>").toInt
          if((choixC4!=1)&&(choixC4!=2)&&(choixC4!=3)) {
            do {
              choixC4= readLine("\n1) Une dose (50ml) - CHF 0.05 " +
                "\n2) Deux doses (100ml) - CHF 0.10  " +
                "\n3) Trois doses (150ml) - CHF 0.15\n>").toInt
            }
            while((choixC4!=1)&&(choixC4!=2)&&(choixC4!=3))
          }
        }
      }

      if(choixC==1) {
        if(café<8) {
          caféIns=true
          typeboisson="Expresso"
          café +=0
        }
        else {
          café = café - 8
          prixboisson=2.00f
          typeboisson="Expresso"
        }
      }
      if(choixC==2) {
        if(café<6) {
          caféIns=true
          typeboisson="Cappuccino"
          café+=0
        }
        if(lait<100) {
          laitIns=true
          typeboisson="Cappuccino"
          lait+=0
        }
        else {
        café = café - 6
        lait = lait - 100
          prixboisson=2.50f
          typeboisson="Cappuccino"
        }
      }
      if((choixC==3)&&(choixLatte==1)) {
        if(café<6) {
          caféIns=true
          typeboisson="Latte (Petit)"
          café+=0
        }
          if(lait<120) {
          laitIns=true
            typeboisson="Latte (Petit)"
            lait+=0
        }
        else {
        café = café - 6
        lait = lait - 120
          prixboisson=2.70f
          typeboisson="Latte (Petit)"
        }
      }
      if((choixC==3)&&(choixLatte==2)) {
        if(café<8) {
          caféIns=true
          typeboisson="Latte (Moyen)"
          café+=0
        }
        if(lait<150) {
          laitIns=true
          typeboisson="Latte (Moyen)"
          lait+=0
        }
        else {
        café = café - 8
        lait = lait - 150
          prixboisson=3.20f
          typeboisson="Latte (Moyen)"
        }
      }
      if((choixC==3)&&(choixLatte==3)) {
        if(café<12) {
          caféIns=true
          typeboisson="Latte (Grand)"
          café+=0
        }
        if(lait<200) {
          laitIns=true
          typeboisson="Latte (Grand)"
          lait+=0
        }
        else {
        café = café - 12
        lait = lait - 200
          prixboisson=3.70f
          typeboisson="Latte (Grand)"
        }
      }
      if(choixC4==1) {
        if(lait<50) {
          laitIns=true
          typelait="une dose de lait (50ml)"
          lait+=0
        }
        else {
        lait = lait - 50
          prixlait=0.05f
          typelait="une dose de lait (50ml)"
        }
      }
      if(choixC4==2) {
        if(lait<100) {
          laitIns=true
          typelait="deux doses de lait (100ml)"
          lait+=0
        }
        else {
        lait = lait - 100
          prixlait=0.10f
          typelait="deux doses de lait (100ml)"
        }
      }
      if(choixC4==3) {
        if(lait<150) {
          laitIns=true
          typelait="trois doses de lait (150ml)"
          lait+=0
        }
        else {
        lait = lait - 150
          prixlait=0.15f
          typelait="trois doses de lait (150ml)"
        }
      }
      if(choixC2==1) {
        sucre = sucre
        prixsucre=0.0f
        typesucre="Pas de sucre"

      }
      if(choixC2==2) {
        if(sucre<5) {
         sucreIns=true
          typesucre="Peu"
          sucre+=0
        }
        else {
        sucre = sucre - 5
          prixsucre=0.10f
          typesucre="Peu"
        }
      }
      if(choixC2==3) {
        if(sucre<10) {
         sucreIns=true
          typesucre="Moyen"
          sucre+=0
        }
        else {
        sucre = sucre - 10
          prixsucre=0.20f
          typesucre="Moyen"
        }
      }
      if(choixC2==4) {
        if(sucre<15) {
          sucreIns=true
          typesucre="Beaucoup"
          sucre+=0
        }
        else {
        sucre = sucre - 15
          prixsucre=0.30f
          typesucre="Beaucoup"
        }
      }
      prixTotal= prixboisson + prixsucre + prixlait
      println("Boisson sélectionnée : " + typeboisson)
      println("Niveau de sucre : " + typesucre)
      if(choixC3==1) {
        println("Lait supplémentaire : " + typelait)
      }
      else {
        println("Lait supplémentaire : Non")
      }
      if((caféIns==true)||(sucreIns==true)||(laitIns==true)) {
        end=false
      if ((caféIns==true)&&(laitIns==true)) {
        double=true
        println("Erreur : Quantité de café et de lait insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        Thread.sleep(3000)
      }
      else {
        laitcafé=false
      }
      if ((caféIns==true)&&(sucreIns==true)) {
        double=true
        println("Erreur : Quantité de sucre et de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        Thread.sleep(3000)
      }
      else {
        cafésucre=false
      }
      if ((sucreIns==true)&&(laitIns==true)) {
        double=true
        println("Erreur : Quantité de sucre et de lait insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        Thread.sleep(3000)
      }

      else {
        laitsucre=false
      }
      while ((laitsucre!=false)&&(laitcafé!=false)&&(cafésucre!=false)) {
        if(laitIns==true) {
          println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
          laitsucre= true
        }

        if(caféIns==true) {
          println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
          laitcafé=true
        }
        if(sucreIns==true){

          println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
           cafésucre=true
        }
      }
        while (double!=true) {
          if(laitIns==true) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            double=true
          }

          if(caféIns==true) {
            println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            double=true
          }
          if(sucreIns==true){
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
            double=true
          }
        café = caféI
        lait = laitI
        sucre=sucreI
          Thread.sleep(3000)
        end2=false
        admin1=0
        }
      }
      else {
        café = café
        lait = lait
        sucre = sucre
      printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f ",prixboisson,prixsucre,prixlait,prixTotal)
      codeTwint=Random.alphanumeric.take(5).mkString

      println("\nVeuillez payer en utilisant TWINT.")
      println("Votre code de paiement est : " + codeTwint)
      println("En attente de validation du paiement...")

      println("Merci ! Votre paiement a été accepté.")
      println("Préparation de votre boisson...")
      println("...")
      println("Votre " +typeboisson+ " est prêt ! Bonne dégustation !")
      Thread.sleep(5000)
        end2=true
      }
    }
    }
   if(choix==2) {
    choixAdmin=readLine("Entrez le code PIN : ******\n>").toInt
    if(choixAdmin==codeAdmin) {

    }
    else {
      do {
      choixAdmin=readLine("Ce n'est pas le bon code, veuillez réessayer !\n>").toInt
      }
      while(choixAdmin!=codeAdmin)
    }
    println("Accès autorisé.")
    println("Stocks : \nPoudre de café : " + café + " \nLait : " + lait + " \nsucre : " + sucre)
    choixStock= readLine("Désirez-vous réapprovisionner les stocks ? : \n1) Oui \n2) Non\n>").toInt
    if((choixStock!=1)&&(choixStock!=2)) {
      do {
        choixStock= readLine("Désirez-vous réapprovisionner les stocks ? : \n1) Oui \n2) Non\n>").toInt
      }
      while((choixStock!=1)&&(choixStock!=2))
    }
    if(choixStock==1) {
      choixStock2=readLine("Réapprovisionner les stocks de : \n1) café \n2) lait \n3) sucre\n>").toInt
      if((choixStock2!=1)&&(choixStock2!=2)&&(choixStock2!=3)) {
        do {
          choixStock2=readLine("Réapprovisionner les stocks de : \n1) café \n2) lait \n3) sucre\n>").toInt
        }
        while((choixStock2!=1)&&(choixStock2!=2)&&(choixStock2!=3))
      }
    }
      if((choixStock2==1)||(choixStock2==2)||(choixStock2==3)) {
        if(choixStock2==1) {
        quantitéstock1=readLine("Combien voulez-vous réapprovisionner de grains de café (en grammes) ?\n>").toDouble
        café=café+quantitéstock1
          choixStock2=readLine("Voulez-vous réapprovisionner un autre stock ? : \n1) Non  \n2) Oui,celui de Lait  \n3) Oui, celui de Sucre\n>").toInt
          if((choixStock2!=1)&&(choixStock2!=2)&&(choixStock2!=3)) {
            do {
              choixStock2=readLine("Voulez-vous réapprovisionner un autre stock ? : \n1) Non  \n2) Oui, celui de Lait  \n3) Oui, celui de Sucre\n>").toInt
            }
            while((choixStock2!=1)&&(choixStock2!=2)&&(choixStock2!=3))
          }
          if(choixStock2==1) {
            end2=true
          }
        }
        if(choixStock2==2) {
          quantitéstock2=readLine("Combien voulez-vous réapprovisionner de lait (en mL) ?\n>").toDouble
          lait = lait + quantitéstock2
          choixStock2=readLine("Voulez-vous réapprovisionner un autre stock ? : \n1) Oui, celui de Café  \n2) Non  \n3) Oui, celui de Sucre\n>").toInt
          if((choixStock2!=1)&&(choixStock2!=2)&&(choixStock2!=3)) {
            do {
              choixStock2=readLine("Voulez-vous réapprovisionner un autre stock ? : \n1) Oui, celui de Café  \n2) Non  \n3) Oui, celui de Sucre\n>").toInt
            }
            while((choixStock2!=1)&&(choixStock2!=2)&&(choixStock2!=3))
          }
          if(choixStock2==2) {
            end2=true
          }
        }
        if(choixStock2==3) {
          quantitéstock3=readLine("Combien voulez-vous réapprovisionner de sucre (en grammes) ?\n>").toDouble
          sucre = sucre + quantitéstock3
          choixStock2=readLine("Voulez-vous réapprovisionner un autre stock ? : \n1) Oui, celui de Café  \n2) Oui, celui de Lait  \n3) Non\n>").toInt
         if((choixStock2!=1)&&(choixStock2!=2)&&(choixStock2!=3)) {
           do {
             choixStock2=readLine("Voulez-vous réapprovisionner un autre stock ? : \n1) Oui, celui de  Café  \n2) Oui, celui Lait  \n3) Non\n>").toInt
           }
           while((choixStock2!=1)&&(choixStock2!=2)&&(choixStock2!=3))
         }
          if(choixStock2==3) {
            end2=true
          }
        }
      }
      println("Réapprovisionnement des stocks...")
      println("Ajout : \nPoudre de café : + "+quantitéstock1+ " \nLait : + "+quantitéstock2+ " \nSucre : + "+quantitéstock3)
      println("Niveaux de stock mis à jour.")
      println("Les nouveaux stocks sont : \nPoudre de café : "+café+" \nLait : "+lait+" \nSucre : "+sucre)
      println("Retour au menu principal...")
      Thread.sleep(5000)
      end2=true


      if(choixStock==2) {
        Thread.sleep(5000)
        end2=true
      }
  }

    if(choix==3) {
    end=true
  }
}
}
}















