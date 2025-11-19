object Nospresso {
  var StockCafe: Double = 0
  var StockSucre: Double = 0
  var StockLait: Double = 0
  var StockCafeReel: Double = 50
  var StockSucreReel: Double = 30
  var StockLaitReel: Double = 500
  var prix: Double = 0
  var taille=""
  var NiveauSucre=""
  var NiveauLait="Non"
  var PrixBase=""
  var PrixSucre=""
  var PrixLait=""
  var Sucre=""
  var LaitSup="0"
  var LaitOuPas=""
  var Latte=""

  def main(args: Array[String]): Unit = {
  }
      var Demande = true
      var boisson = ""
      while (Demande) {
        println("        Nospresso Café")
        println("Veuillez sélectionner votre mode :")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        println(">")

        val mode = scala.io.StdIn.readLine()
        if (mode == "1") {
          println("Mode Client")
          val Mode1=true
          Demande = false
          while (Mode1){

          var DemandeBoisson = true
          while (DemandeBoisson) {

            prix=0
            StockCafe=0
            StockSucre=0
            StockLait=0

            println("Sélectionnez votre boisson : :")
            println("1) Expresso - CHF 2.00")
            println("2) Cappuccino - CHF 2.50")
            println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
            println(">")

            boisson = scala.io.StdIn.readLine()

            if (boisson == "1") {
              prix += 2.00
              StockCafe += 8
              PrixBase=" CHF 2.00"
              DemandeBoisson = false
            } else if (boisson == "2") {
              prix += 2.50
              StockCafe += 6
              StockLait += 100
              PrixBase=" CHF 2.50"
              DemandeBoisson = false
            } else if (boisson == "3") {
              var DemandeLatte = true
              while (DemandeLatte) {
                println("Quelle taille de Latte souhaitez-vous ?")
                println("1) Petit")
                println("2) Moyen")
                println("3) Grand")
                println(">")

                Latte = scala.io.StdIn.readLine()

                if (Latte == "1") {
                  prix += 2.70
                  StockCafe += 6
                  StockLait += 120
                  taille=" (Petit)"
                  PrixBase=" CHF 2.70"
                  DemandeLatte = false
                } else if (Latte == "2") {
                  prix += 3.20
                  StockCafe += 8
                  StockLait += 150
                  taille=" (Moyen)"
                  PrixBase=" CHF 3.20"
                  DemandeLatte = false
                } else if (Latte == "3") {
                  prix += 3.70
                  StockCafe += 12
                  StockLait += 200
                  taille=" (Grand)"
                  PrixBase=" CHF 3.70"
                  DemandeLatte = false
                } else {
                  println("Sélection invalide, veuillez réessayer. 4")
                }
              }
              DemandeBoisson = false
            } else {
              println("Sélection invalide, veuillez réessayer. 3")
            }
          }

          var DemandeSucre = true
          while (DemandeSucre) {
            println("Souhaitez-vous ajoutez du sucre ? :")
            println("1) Sans sucre")
            println("2) Peu (5g) - CHF 0.10")
            println("3) Moyen (10g) - CHF 0.20")
            println("4) Beaucoup (15g) - CHF 0.30")
            println(">")

            Sucre = scala.io.StdIn.readLine()

            if (Sucre == "1") {
              NiveauSucre= " Sans sucre"
              DemandeSucre = false
            } else if (Sucre == "2") {
              prix += 0.10
              StockSucre += 5
              NiveauSucre= " Peu (5g)"
              PrixSucre=" + CHF 0.10"
              DemandeSucre = false
            } else if (Sucre == "3") {
              prix += 0.20
              StockSucre += 10
              NiveauSucre= " Moyen (10g)"
              PrixSucre=" + CHF 0.20"
              DemandeSucre = false
            } else if (Sucre == "4") {
              prix += 0.30
              StockSucre += 15
              NiveauSucre= " Beaucoup (15g)"
              PrixSucre=" + CHF 0.30"
              DemandeSucre = false
            } else
            {
              println("Sélection invalide, veuillez réessayer. ")
            }
          }

          if (boisson == "2" || boisson == "3") {
                  var DemandeLaitouPas = true

                  while (DemandeLaitouPas) {
                    println("Souhaitez-vous ajouter du lait en supplément ?")
                    println("(Disponible uniquement pour Cappuccino et Latte")
                    println("1) Oui")
                    println("2) Non")
                    println(">")

                    LaitOuPas = scala.io.StdIn.readLine()
                    if (LaitOuPas == "1") {
                      var DemandeSupLait = true
                      while (DemandeSupLait) {
                        println("Combien de dose ? (Maximum 3) :")
                        println(">")
                        LaitSup = scala.io.StdIn.readLine()

                        if (LaitSup == "0") {
                          NiveauLait=" Non"
                          DemandeSupLait = false
                        } else if (LaitSup == "1") {
                          prix += 0.05
                          StockLait += 50
                          NiveauLait=" Une dose"
                          PrixLait= " + CHF 0.05"
                          DemandeSupLait = false
                        } else if (LaitSup == "2") {
                          prix += 0.10
                          StockLait += 100
                          NiveauLait=" Deux doses"
                          PrixLait= " + CHF 0.10"
                          DemandeSupLait = false
                        } else if (LaitSup == "3") {
                          prix += 0.15
                          StockLait += 150
                          NiveauLait=" Trois doses"
                          PrixLait= " + CHF 0.15"
                          DemandeSupLait = false

                        } else {
                          println("Merci de choisir une quantité de lait entre 0 et 3.")
                        }
                    }
                    DemandeLaitouPas = false
                  } else if (LaitOuPas == "2") {
                      NiveauLait=" Non"
                    DemandeLaitouPas = false
                  } else {
                    println("Sélection invalide, veuillez réessayer .")

                  }
        }}
            if (boisson=="1"){
              println("Boisson sélecionnée : Expresso")
            }
            else if (boisson=="2"){
              println("Boisson sélecionnée : Cappuccino")
            }
            else if (boisson=="3"){
              println("Boisson sélecionnée : Latte"+taille)
            }

            println("Niveau de sucre :"+NiveauSucre)
            if (boisson=="2" || boisson=="3"){
              println("Lait supplémentaire :"+ NiveauLait)
            }
            println("")
            // Vérification des stocks
            if (StockLait > StockLaitReel && Latte == "2") {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              println("")

            } else if (StockCafe > StockCafeReel && Latte == "2") {
              println("Erreur : Quantité de Café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              println("")
            } else if (StockLait > StockLaitReel && Latte == "3") {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              println("")
            } else if (StockCafe > StockCafeReel && Latte == "3") {
              println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              println("")

            } else if (StockCafe > StockCafeReel && Latte == "1") {
              println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              println("")
            } else if (StockLait > StockLaitReel && Latte == "1") {
                println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                println("")
            } else if (StockSucre > StockSucreReel && Latte == "1") {
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  println("")

            } else if (StockCafe > StockCafeReel && (boisson == "1" || boisson == "2")) {
              println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              println("")
            } else if (StockLait > StockLaitReel && (boisson == "1" || boisson == "2")) {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              println("")
            } else if (StockSucre > StockSucreReel && (boisson == "1" || boisson == "2")) {
              println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
              println("")

            } else {val caracterepossible = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
              var CodeTwint = ""
              for (_ <- 1 to 5) {
                val randomIndex = scala.util.Random.nextInt(caracterepossible.length)
                CodeTwint = CodeTwint + caracterepossible(randomIndex)
              }

              if(Sucre=="1"&&LaitSup=="0"){
                println("Prix total : CHF "+ f"$prix%.2f")
                println("")
              }
              else if(Sucre=="1"&&LaitOuPas=="2"){
                println("Prix total : CHF "+ f"$prix%.2f")
                println("")
              }
              else if(boisson=="1"&&Sucre=="1"){
                println("Prix total : CHF "+ f"$prix%.2f")
                println("")
              }
              else{
                println("Prix total :"+ PrixBase + PrixSucre + PrixLait + " = CHF " + f"$prix%.2f")
                println("")
              }
              println("Veuillez payer en utilisant Twint.")
              println(s"Votre code de paiement est : $CodeTwint")

              println("En attente de validation du paiement...")
              Thread.sleep(3000) // Pause de 3 secondes (3000 millisecondes)
              println("")
              println("Merci ! Votre paiement a été accepté.")

              println("")
              println("Préparation de votre boisson...")
              println("[...]")
              Thread.sleep(5000) // Pause de 5 secondes (5000 millisecondes)
              if (boisson == "1") {
                println("Votre Expresso est prêt ! Bonne dégustation !")
                println("")
              } else if (boisson == "2") {
                println("Votre Cappuccino est prêt ! Bonne dégustation !")
                println("")
              } else if (boisson == "3") {
                println("Votre Latte est prêt ! Bonne dégustation !")
                println("")
              }
              StockLaitReel -= StockLait
              StockCafeReel -= StockCafe
              StockSucreReel -= StockSucre
              PrixBase=""
              PrixSucre=""
              PrixLait=""
              taille=""
              NiveauSucre=""
              NiveauLait="Non"
              PrixBase=""
              PrixSucre=""
              PrixLait=""
              Sucre=""
              LaitSup=""
              LaitOuPas=""
              Latte=""
            }
          }

        }
        else if (mode == "2") {
          println("Mode Admin")
          println("Entrez le code PIN:")
          var Code=true

          while (Code){
            val CodeEntrer = scala.io.StdIn.readLine()
          if (CodeEntrer == "434343"){
            println("Accès autorisé.")
            Code=false
            println("Stocks:")
            println(" Poudre de café:" + StockCafeReel + "g")
            printf(" Lait: %.2f L\n", StockLaitReel / 1000.0)
            println(" Sucre:" + StockSucreReel + "g")
            println("")
            println("Réapprovisionnement des stocks...")
            println("Ajout :")

            //Il est évident que les stocks seront toujours pleins car l'on relance le programme
            //mais je me suis basé sur la modification demandé par le prof sur moodle
            //comme il a dit c'est un exercice et tout ne doit pas faire sens.

            var rechargecafe=true
            while (rechargecafe){
            println(" Poudre de café:")
              val Cafe = scala.io.StdIn.readLine().toDouble
              if (Cafe <= 50 && Cafe>0) {
                StockCafeReel=Cafe
                rechargecafe=false
              }
              else{
                println("La quantité entrée dépasse la capacité du réservoir.")
                println("Merci de rentrer une quantité comprise entre 0 et 50g")
              }
            }

              var rechargelait=true
              while (rechargelait){
                println(" Lait:")
                val Lait = scala.io.StdIn.readLine().toDouble
                if (Lait <= 0.5 && Lait>0) {
                  StockLaitReel=Lait*1000
                  rechargelait=false
                }
                else{
                  println("La quantité entrée dépasse la capacité du réservoir.")
                  println("Merci de rentrer une quantité comprise entre 0 et 0.5L")}}

              var rechargesucre=true
              while (rechargesucre){
                println(" Sucre:")
                val Sucre = scala.io.StdIn.readLine().toDouble
                if (Sucre <= 30 && Sucre >0) {
                  StockSucreReel=Sucre
                  rechargesucre=false
                }
                else{
                  println("La quantité entrée dépasse la capacité du réservoir.")
                  println("Merci de rentrer une quantité comprise entre 0 et 30g")
                }
                  }

            println("Niveaux de stock mis à jour.")
            println("Retour au menu prinicipal...")
          }
          else{
            println("CodePin erroné, veuillez réessayer.")
          }
          }
        }
        else if (mode == "3") {
          println("Au revoir")
          Demande = false
        } else {
          println("Sélection invalide, veuillez réessayer." )
        }
      }
    }
