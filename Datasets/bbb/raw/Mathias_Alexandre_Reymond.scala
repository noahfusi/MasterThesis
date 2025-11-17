object Main {
  def main(args: Array[String]): Unit = {
    import scala.io.StdIn.readLine
    import scala.math._
    import scala.util.Random

    val nbMachines = 5
    var end=false
    var end2=false
    var café=50.0
    var sucre=30.0
    var lait= 500.0
    var choix=0
    var end3 = false
    var choixAdmin=""
    var machinePins = Array.fill(nbMachines)("434343")
    var mcodePin=0
    var nvcodePin = ""
    var essaiPin=0
    var caféIns=false
    var sucreIns=false
    var laitIns=false
    var caféstocks = Array.fill(nbMachines)(50)
    var sucrestocks = Array.fill(nbMachines)(30)
    var laitstocks = Array.fill(nbMachines)(500)

    def validatePin(choixmachine: Int, machinePins: Array[String] ): Boolean = {
      choixAdmin=readLine("\nEntrez le code Pin : ****** \n>")
      if(choixAdmin==machinePins(choixmachine-1)) {
        true
      }
      else {
        false
      }
    }

    def updatePin(choixmachine: Int, machinePins: Array[String] ) : Unit = {
      println("Mise à jour du code PIN pour la Machine " + choixmachine + ".")
      nvcodePin=readLine("Entrez un nouveau code Pin à 6 chiffres \n>")
      if (nvcodePin.length !=6) {
        do {
          nvcodePin=readLine("Entrez un nouveau code Pin à 6 chiffres \n>")
        }
        while(nvcodePin.length!=6)
      }

      machinePins(choixmachine-1) = nvcodePin
      println("Le code PIN a été mis à jour avec succès.")
      println("Retour au menu principal...")
    }


    def serveClient(choixmachine: Int,caféstocks: Array[Int], sucrestocks: Array[Int], laitstocks: Array[Int]) : Boolean = {
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
        if(caféstocks(choixmachine-1)<8) {
          caféIns=true
          typeboisson="Expresso"
          caféstocks(choixmachine-1) +=0
        }
        else {
          caféstocks(choixmachine-1) = caféstocks(choixmachine-1) - 8
          prixboisson=2.00f
          typeboisson="Expresso"
        }
      }
      if(choixC==2) {
        if(caféstocks(choixmachine-1)<6) {
          caféIns=true
          typeboisson="Cappuccino"
          caféstocks(choixmachine-1)+=0
        }
        if(laitstocks(choixmachine-1)<100) {
          laitIns=true
          typeboisson="Cappuccino"
          laitstocks(choixmachine-1)+=0
        }
        else {
          caféstocks(choixmachine-1) = caféstocks(choixmachine-1) - 6
          laitstocks(choixmachine-1) = laitstocks(choixmachine-1) - 100
          prixboisson=2.50f
          typeboisson="Cappuccino"
        }
      }
      if((choixC==3)&&(choixLatte==1)) {
        if(caféstocks(choixmachine-1)<6) {
          caféIns=true
          typeboisson="Latte (Petit)"
          caféstocks(choixmachine-1)+=0
        }
        if(laitstocks(choixmachine-1)<120) {
          laitIns=true
          typeboisson="Latte (Petit)"
          laitstocks(choixmachine-1)+=0
        }
        else {
          caféstocks(choixmachine-1) = caféstocks(choixmachine-1) - 6
          laitstocks(choixmachine-1) = laitstocks(choixmachine-1) - 120
          prixboisson=2.70f
          typeboisson="Latte (Petit)"
        }
      }
      if((choixC==3)&&(choixLatte==2)) {
        if(caféstocks(choixmachine-1)<8) {
          caféIns=true
          typeboisson="Latte (Moyen)"
          caféstocks(choixmachine-1)+=0
        }
        if(laitstocks(choixmachine-1)<150) {
          laitIns=true
          typeboisson="Latte (Moyen)"
          laitstocks(choixmachine-1)+=0
        }
        else {
          caféstocks(choixmachine-1) = caféstocks(choixmachine-1) - 8
          laitstocks(choixmachine-1) = laitstocks(choixmachine-1) - 150
          prixboisson=3.20f
          typeboisson="Latte (Moyen)"
        }
      }
      if((choixC==3)&&(choixLatte==3)) {
        if(caféstocks(choixmachine-1)<12) {
          caféIns=true
          typeboisson="Latte (Grand)"
          caféstocks(choixmachine-1)+=0
        }
        if(laitstocks(choixmachine-1)<200) {
          laitIns=true
          typeboisson="Latte (Grand)"
          laitstocks(choixmachine-1)+=0
        }
        else {
          caféstocks(choixmachine-1) = caféstocks(choixmachine-1) - 12
          laitstocks(choixmachine-1) = laitstocks(choixmachine-1) - 200
          prixboisson=3.70f
          typeboisson="Latte (Grand)"
        }
      }
      if(choixC4==1) {
        if(laitstocks(choixmachine-1)<50) {
          laitIns=true
          typelait="une dose de lait (50ml)"
          laitstocks(choixmachine-1)+=0
        }
        else {
          laitstocks(choixmachine-1) = laitstocks(choixmachine-1) - 50
          prixlait=0.05f
          typelait="une dose de lait (50ml)"
        }
      }
      if(choixC4==2) {
        if(laitstocks(choixmachine-1)<100) {
          laitIns=true
          typelait="deux doses de lait (100ml)"
          laitstocks(choixmachine-1)+=0
        }
        else {
          laitstocks(choixmachine-1) = laitstocks(choixmachine-1) - 100
          prixlait=0.10f
          typelait="deux doses de lait (100ml)"
        }
      }
      if(choixC4==3) {
        if(laitstocks(choixmachine-1)<150) {
          laitIns=true
          typelait="trois doses de lait (150ml)"
          laitstocks(choixmachine-1)+=0
        }
        else {
          laitstocks(choixmachine-1) = laitstocks(choixmachine-1) - 150
          prixlait=0.15f
          typelait="trois doses de lait (150ml)"
        }
      }
      if(choixC2==1) {
        sucrestocks(choixmachine-1) = sucrestocks(choixmachine-1)
        prixsucre=0.0f
        typesucre="Pas de sucre"

      }
      if(choixC2==2) {
        if(sucrestocks(choixmachine-1)<5) {
          sucreIns=true
          typesucre="Peu"
          sucrestocks(choixmachine-1)+=0
        }
        else {
          sucrestocks(choixmachine-1) = sucrestocks(choixmachine-1) - 5
          prixsucre=0.10f
          typesucre="Peu"
        }
      }
      if(choixC2==3) {
        if(sucrestocks(choixmachine-1)<10) {
          sucreIns=true
          typesucre="Moyen"
          sucrestocks(choixmachine-1)+=0
        }
        else {
          sucrestocks(choixmachine-1) = sucrestocks(choixmachine-1) - 10
          prixsucre=0.20f
          typesucre="Moyen"
        }
      }
      if(choixC2==4) {
        if(sucrestocks(choixmachine-1)<15) {
          sucreIns=true
          typesucre="Beaucoup"
          sucrestocks(choixmachine-1)+=0
        }
        else {
          sucrestocks(choixmachine-1) = sucrestocks(choixmachine-1) - 15
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
          println("Erreur : Quantité de café et de lait insuffisante pour préparer la boisson sélectionnée.")
          Thread.sleep(3000)
        }
        else {
          laitcafé=false
        }
        if ((caféIns==true)&&(sucreIns==true)) {
          double=true
          println("Erreur : Quantité de sucre et de café insuffisante pour préparer la boisson sélectionnée.")
          Thread.sleep(3000)
        }
        else {
          cafésucre=false
        }
        if ((sucreIns==true)&&(laitIns==true)) {
          double=true
          println("Erreur : Quantité de sucre et de lait insuffisante pour préparer la boisson sélectionnée.")
          Thread.sleep(3000)
        }

        else {
          laitsucre=false
        }
        while ((laitsucre!=false)&&(laitcafé!=false)&&(cafésucre!=false)) {
          if(laitIns==true) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            laitsucre= true
          }

          if(caféIns==true) {
            println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
            laitcafé=true
          }
          if(sucreIns==true){

            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            cafésucre=true
          }
        }
        while (double!=true) {
          if(laitIns==true) {
            println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
            double=true
          }

          if(caféIns==true) {
            println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
            double=true
          }
          if(sucreIns==true){
            println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
            double=true
          }
          caféstocks(choixmachine-1) = caféstocks(choixmachine-1)
          laitstocks(choixmachine-1) = laitstocks(choixmachine-1)
          sucrestocks(choixmachine-1) = sucrestocks(choixmachine-1)
          end2=false
          admin1=0
        }
        return false
      }
      else {
        caféstocks(choixmachine-1) = caféstocks(choixmachine-1)
        laitstocks(choixmachine-1) = laitstocks(choixmachine-1)
        sucrestocks(choixmachine-1) = sucrestocks(choixmachine-1)
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

      true
    }

    def restockMachine(choixmachine: Int,caféstocks: Array[Int], sucrestocks: Array[Int], laitstocks: Array[Int]):Unit = {
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
      var choixAdmin=""
      var choixStock=0
      var choixStock2=0
      var quantitéstock1=0
      var quantitéstock2=0
      var quantitéstock3=0
      var laitcafé=false
      var cafésucre=false
      var laitsucre=false
      var double=false
      var essaiPin = 0
      var end3 = false

      println("Stocks : \nPoudre de café : " + caféstocks(choixmachine-1) + " grammes " + " \nLait : " + laitstocks(choixmachine-1)/1000.toDouble + " litres " + " \nsucre : " + sucrestocks(choixmachine-1) + " grammes ")
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
          quantitéstock1=readLine("Combien voulez-vous réapprovisionner de grains de café (en grammes) ?\n>").toInt
          if(quantitéstock1<0) {
            do {
              quantitéstock1=readLine("Combien voulez-vous réapprovisionner de grains de café (en grammes) ?\n>").toInt
            }
            while(quantitéstock1<0)
          }
          caféstocks(choixmachine-1) = caféstocks(choixmachine-1) + quantitéstock1
          choixStock2=readLine("Voulez-vous réapprovisionner un autre stock ? : \n1) Non  \n2) Oui,celui de Lait  \n3) Oui, celui de Sucre\n>").toInt
          if((choixStock2!=1)&&(choixStock2!=2)&&(choixStock2!=3)) {
            do {
              choixStock2=readLine("Voulez-vous réapprovisionner un autre stock ? : \n1) Non  \n2) Oui, celui de Lait  \n3) Oui, celui de Sucre\n>").toInt
            }
            while((choixStock2!=1)&&(choixStock2!=2)&&(choixStock2!=3))
          }
          if(choixStock2==1) {
            end2=true
            end3=true
          }
        }
        if(choixStock2==2) {
          quantitéstock2=readLine("Combien voulez-vous réapprovisionner de lait (en mL) ?\n>").toInt
          if(quantitéstock2<0){
            do {
              quantitéstock2=readLine("Combien voulez-vous réapprovisionner de lait (en mL) ?\n>").toInt
            }
            while(quantitéstock2<0)
          }
          laitstocks(choixmachine-1) = laitstocks(choixmachine-1) + quantitéstock2
          choixStock2=readLine("Voulez-vous réapprovisionner un autre stock ? : \n1) Oui, celui de Café  \n2) Non  \n3) Oui, celui de Sucre\n>").toInt
          if((choixStock2!=1)&&(choixStock2!=2)&&(choixStock2!=3)) {
            do {
              choixStock2=readLine("Voulez-vous réapprovisionner un autre stock ? : \n1) Oui, celui de Café  \n2) Non  \n3) Oui, celui de Sucre\n>").toInt
            }
            while((choixStock2!=1)&&(choixStock2!=2)&&(choixStock2!=3))
          }
          if(choixStock2==2) {
            end2=true
            end3=true
          }
        }
        if(choixStock2==3) {
          quantitéstock3=readLine("Combien voulez-vous réapprovisionner de sucre (en grammes) ?\n>").toInt
          if(quantitéstock3<0){
            do{
              quantitéstock3=readLine("Combien voulez-vous réapprovisionner de sucre (en grammes) ?\n>").toInt
            }
            while(quantitéstock3<0)
          }
          sucrestocks(choixmachine-1) = sucrestocks(choixmachine-1) + quantitéstock3
          choixStock2=readLine("Voulez-vous réapprovisionner un autre stock ? : \n1) Oui, celui de Café  \n2) Oui, celui de Lait  \n3) Non\n>").toInt
          if((choixStock2!=1)&&(choixStock2!=2)&&(choixStock2!=3)) {
            do {
              choixStock2=readLine("Voulez-vous réapprovisionner un autre stock ? : \n1) Oui, celui de  Café  \n2) Oui, celui Lait  \n3) Non\n>").toInt
            }
            while((choixStock2!=1)&&(choixStock2!=2)&&(choixStock2!=3))
          }
          if(choixStock2==3) {
            end2=true
            end3=true
          }
        }
      }
      println("Réapprovisionnement des stocks...")
      println("Ajout : \nPoudre de café : + "+quantitéstock1+ " grammes " + " \nLait : + "+quantitéstock2 + " mL " + " \nSucre : + "+quantitéstock3 + " grammes")
      println("Niveaux de stock mis à jour.")
      println("Les nouveaux stocks sont : \nPoudre de café : "+caféstocks(choixmachine-1)+ " grammes" + " \nLait : "+laitstocks(choixmachine-1)/1000.toDouble + " litres" + " \nSucre : "+sucrestocks(choixmachine-1) + " grammes")
      println("Retour au menu principal...")
      Thread.sleep(2000)
      end2=true
      end3=true


      if(choixStock==2) {
        Thread.sleep(2000)
        end2=true
        end3=true
      }
    }



    while(end!=true) {
      var choixmachine=0
      choix = readLine("Nospresso Café, Veuillez séléctionner votre mode : \n1) Client \n2) Admin \n3) Quitter \n>").toInt
      if(choix==3){

      }
      else {
        choixmachine = readLine("Quelle machine désirez-vous utiliser : \n1) machine 1 \n2) machine 2 \n3) machine 3 \n4) machine 4 \n5) machine 5 \n>").toInt
        if((choixmachine!=1)&&(choixmachine!=2)&&(choixmachine!=3)&&(choixmachine!=4)&&(choixmachine!=5)){
          do {
            choixmachine = readLine("Quelle machine désirez-vous utiliser : \n1) machine 1 \n2) machine 2 \n3) machine 3 \n4) machine 4 \n5) machine 5 \n>").toInt
          }
          while((choixmachine!=1)&&(choixmachine!=2)&&(choixmachine!=3)&&(choixmachine!=4)&&(choixmachine!=5))
        }
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
        var choixAdmin=""
        var choixStock=0
        var choixStock2=0
        var quantitéstock1=0.0
        var quantitéstock2=0.0
        var quantitéstock3=0.0
        var laitcafé=false
        var cafésucre=false
        var laitsucre=false
        var double=false
        var essaiPin = 0
        var end3 = false
        if (choix==1) {
          while(end2!=true) {
            val erreurstocks = serveClient(choixmachine: Int,caféstocks: Array[Int], sucrestocks: Array[Int], laitstocks: Array[Int])
            if (erreurstocks) {
              end2=true

            }
            else {
              println("Veuillez sélectionner une autre machine !")
              Thread.sleep(3000)
              end2=true

            }
          }
        }

        if(choix==2) {
          print("Machine sélectionnée (1-5) >  " + choixmachine)
          while(!end3){
            while(essaiPin<3){
              val validPin = validatePin(choixmachine,machinePins)
              if(validPin) {
                essaiPin = 3
                println("Accès accordé à la machine "+ choixmachine)
                mcodePin=readLine("Voulez-vous mettre à jour le code PIN? \n1)Oui \n2)Non \n>").toInt
                if((mcodePin!=1)&&(mcodePin!=2)) {
                  do {
                    mcodePin=readLine("Voulez-vous mettre à jour le code PIN? \n1)Oui \n2)Non \n>").toInt
                  }
                  while((mcodePin!=1)&&(mcodePin!=2))
                }
                if(mcodePin==1) {
                  updatePin(choixmachine,machinePins)
                  essaiPin=3
                  Thread.sleep(3000)
                  end3=true
                }

                else {
                  restockMachine(choixmachine: Int,caféstocks: Array[Int], sucrestocks: Array[Int], laitstocks: Array[Int])
                  end2=true
                  end3=true
                }
              }
              else{
                essaiPin+=1
                println("Code PIN incorrect. " + (3-essaiPin) + " tentatives restantes.")
                if(essaiPin==3){
                  println("Trop de tentatives échouées. Fin du programme.")
                  end3 = true
                  end = true
                }
              }
            }
          }
        }
      }
      if(choix==3) {
        println("Au revoir..")
        end = true

      }
    }
  }
}