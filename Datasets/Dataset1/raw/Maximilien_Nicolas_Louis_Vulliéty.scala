import scala.util.Random
import scala.io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {
  }

  //variables nécessaire pour le stock (en gr et L)
  var stockcafe: Int = 50
  var stocksucre: Int = 30
  var stocklait: Double = 0.5

  //code pin correct
  val codepin: String = "434343"

  // Menu principal
  var running = true
  while (running) {
    println("Nospresso Café\n\nVeuillez sélectionner votre mode :")
    println("1) Client\n2) Admin\n3) Quitter")
    print(">")
    val choixmode = readLine().trim

    if (choixmode == "1") {
      var boissonselectionnee = false
      while (!boissonselectionnee){ //while permet de retourner au menu boisson si les stocks sont insuffisant
        println("1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (petit), CHF 3.20(moyen), CHF 3.70(grand)")
        print(">")
        val choixboisson = readLine().trim
        var boisson = ""
        var prixdebase = 0.0
        var cafenecessaire = 0
        var laitnecessaire = 0.0

        if (choixboisson == "1") {
          boisson = "Expresso"
          prixdebase = 2.0
          cafenecessaire = 8
        } else if (choixboisson == "2") {
          boisson = "Cappuccino"
          prixdebase = 2.5
          cafenecessaire = 6
          laitnecessaire = 0.1
        } else if (choixboisson == "3") {
          boisson = "Latte"
          var taillevalide = false
          while(!taillevalide) {
            println("\nVeuillez chosir la taille de votre Latte :")
            println("1) petit - CHF 2.70\n2) moyen - CHF 3.20\n3) grand - CHF 3.70")
            println(">")
            val choixtaille = readLine().trim
            if(choixtaille == "1") {
              prixdebase = 2.7
              cafenecessaire = 6
              laitnecessaire = 0.12
              taillevalide = true
            } else if(choixtaille == "2") {
              prixdebase = 3.2
              cafenecessaire = 8
              laitnecessaire = 0.15
              taillevalide = true
            } else if(choixtaille == "3") {
              prixdebase = 3.7

              cafenecessaire = 12
              laitnecessaire = 0.2
              taillevalide = true
            } else {
              println("Taille invalide. Veuillez choisir une taille valide.")
            }
          }
        }
        if (boisson != ""){
          var quantitesucre = -1
          while (quantitesucre == -1) {
            println("\nSouhaitez-vous ajouter du sucre ?")
            println("1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
            println(">")
            val choixsucre = readLine().trim
            if(choixsucre == "1") quantitesucre = 0
            else if(choixsucre =="2") quantitesucre = 5
            else if(choixsucre =="3") quantitesucre = 10
            else if(choixsucre =="4") quantitesucre = 15
            else println("Entrée invalide. Veuillez sélectionner une option valide.")
          }

          var doselait = 0
          if(boisson == "Cappuccino" || boisson == "Latte"){
            var veutdulait = false
            var reponselaitvalide = false
            while (!reponselaitvalide) {
              println("\nSouhaitez-vous ajouter du lait en supplément ?")
              println("1) Oui\n2) Non")
              println(">")
              val reponselait = readLine().trim
              if(reponselait == "1"){
                veutdulait = true
                reponselaitvalide = true
              } else if (reponselait == "2") {
                veutdulait = false
                reponselaitvalide = true
              } else {
                println("Entrée invalide. Veuillez sélectionner une entrée valide.")
              }
            }
            if(veutdulait) {
              var quantitelaitvalide = false
              while(!quantitelaitvalide) {
                println("Combien de dose(s) ?")
                println(">")
                val reponsedosedelait = readLine().trim
                if(reponsedosedelait == "1") { doselait = 1
                  quantitelaitvalide = true
                } else if(reponsedosedelait == "2"){ doselait = 2
                  quantitelaitvalide = true
                } else if(reponsedosedelait == "3") { doselait = 3
                  quantitelaitvalide = true
                } else {
                  println("Entrée invalide. Veuillez sélectionner une option valide.")
                }
              }
            }
          }
          //Prix de la boisson
          val prixsucre = if(quantitesucre == 5) 0.1 else if(quantitesucre == 10) 0.2 else if(quantitesucre == 15) 0.3 else 0.0
          val prixlait = doselait*0.05
          val prixtotal = prixdebase + prixsucre + prixlait

          var niveaudesucre = ""
          if(quantitesucre == 0){
            niveaudesucre = "Sans sucre"
          } else if(quantitesucre == 5){
            niveaudesucre = "Peu (5g)"
          } else if(quantitesucre == 10){
            niveaudesucre = "Moyen (10g)"
          } else {
            niveaudesucre = "Beaucoup (15g)"
          }

          var niveaulait = ""
          if(doselait == 0){
            niveaulait = "Non"
          } else {
            niveaulait = "Oui"
          }

          println(s"Boisson sélectionnée : $boisson")
          println(s"Niveau de sucre : $niveaudesucre")
          println(s"Lait supplémentaire : $niveaulait")
          println(f"Prix total : CHF $prixdebase%.2f + CHF $prixsucre%.2f + CHF $prixlait%.2f = $prixtotal%.2f")

          //stocks nécessaire suffisant ?
          if(stockcafe >= cafenecessaire && stocksucre >= quantitesucre && stocklait >= laitnecessaire + (doselait*0.05)) {

            //mise à jour des stocks
            stockcafe -= cafenecessaire
            stocksucre -= quantitesucre
            stocklait -= laitnecessaire + (doselait*0.05)

            println("\nVeuillez payer en utilisant Twint")

            //code de paiement
            val caracteres = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
            val random = new Random()
            var codetwint = ""

            for (_ <- 1 to 5) {

              val caracteresaleatoire = caracteres(random.nextInt(caracteres.length))
              codetwint += caracteresaleatoire
            }
            println(s"Votre code de paiement est : $codetwint")
            Thread.sleep(3000)
            println("Paiement confirmé.\nPréparation de votre boisson...")
            Thread.sleep(2000)
            println(s"Votre $boisson est prêt! Bonne dégustation !")
            boissonselectionnee = true // on sort de la boucle while créé au début qui nous renvoyait au menu boisson si il y avait une insuffisance de stock

          } else {
            println("\nStock insuffisant pour préparer votre boisson. Veuillez essayer une autre option.")
          }
        }
      }
      //Mode admin et réapprovisionement des stocks
    } else if(choixmode == "2") {
      println("\nEntrez le code PIN : ******")
      println(">")
      val pinchoisit = readLine().trim
      if(pinchoisit == codepin) {
        println(s"Accès autorisé.\nStocks actuels:\nCafé : $stockcafe g\nSucre : $stocksucre g\nLait : $stocklait L")
        println("Voulez-vous réapprovisionner les stocks ?\n1) Oui\n2) Non")
        println(">")
        if(readLine().trim == "1"){
          println("Quantité de café à ajouter (en g) :")
          var cafeaajouter = readLine().trim
          stockcafe = stockcafe + cafeaajouter.toInt
          println("Quantité de sucre à ajouter (en g) :")
          var sucreaajouter  = readLine().trim
          stocksucre = stocksucre + sucreaajouter.toInt
          println("Quantité de lait à ajouter (en L) :")
          var laitaajouter = readLine().toDouble
          stocklait = stocklait + laitaajouter
        }
      }
    }
  }
}