object Nospresso {
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
  var Mode1=false
  var run=true

  val nbMachines = 5
  var StockCafe = Array.fill(nbMachines)(0)
  var StockSucre = Array.fill(nbMachines)(0)
  var StockLait = Array.fill(nbMachines)(0)
  var StockCafeReel = Array.fill(nbMachines)(0)
  var StockSucreReel = Array.fill(nbMachines)(0)
  var StockLaitReel = Array.fill(nbMachines)(0)
  var machinePins = Array.fill(nbMachines)("434343")
  var machineId1 = 0

  // Initialisation des codes PIN
  machinePins(0) = "434343"  // Machine 1
  machinePins(1) = "434343"  // Machine 2
  machinePins(2) = "434343"  // Machine 3
  machinePins(3) = "434343"  // Machine 4
  machinePins(4) = "434343"  // Machine 5


  StockCafe(0) = 0  // Machine 1
  StockSucre(0) = 0
  StockLait(0) = 0
  StockCafeReel(0) = 50
  StockSucreReel(0) = 30
  StockLaitReel(0) = 500

  StockCafe(1) = 0  // Machine 2
  StockSucre(1) = 0
  StockLait(1) = 0
  StockCafeReel(1) = 50
  StockSucreReel(1) = 30
  StockLaitReel(1) = 500

  StockCafe(2) = 0  // Machine 3
  StockSucre(2) = 0
  StockLait(2) = 0
  StockCafeReel(2) = 50
  StockSucreReel(2) = 30
  StockLaitReel(2) = 500

  StockCafe(3) = 0  // Machine 4
  StockSucre(3) = 0
  StockLait(3) = 0
  StockCafeReel(3) = 50
  StockSucreReel(3) = 30
  StockLaitReel(3) = 500

  StockCafe(4) = 0  // Machine 5
  StockSucre(4) = 0
  StockLait(4) = 0
  StockCafeReel(4) = 50
  StockSucreReel(4) = 30
  StockLaitReel(4) = 500

  //Def
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var essais = 0
    val essaisMax = 3
    var BonCode = false

    while (essais < essaisMax && !BonCode) {
      println("Entrez le code PIN : ")
      val pin = scala.io.StdIn.readLine()
      if (pin == machinePins(machineId)) {
        BonCode = true
      } else {
        essais += 1
        val essaisRestants = essaisMax - essais
        if (essaisRestants > 0) {
          println(s"Code PIN incorrect. $essaisRestants tentatives restantes.")
        }
      }
    }

    if (!BonCode) {
      println("Trop de tentatives échouées. Fin du programme.")
      System.exit(1)  // Terminer le programme
    }

    BonCode
  }


  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var CodeAccepter = false
    while (!CodeAccepter) {
      println(s"Mise à jour du code PIN pour la Machine ${machineId + 1}.")
      println("Entrez un nouveau code PIN à 6 chiffres : ")
      val NouveauCode = scala.io.StdIn.readLine()
      if (NouveauCode.length == 6 && NouveauCode.forall(_.isDigit)) {
        machinePins(machineId) = NouveauCode
        println(s"Le code PIN a été mis à jour avec succès.")
        println("Retour au menu principal...")
        println("")
        CodeAccepter = true
      } else {
        println("Le code PIN doit comporter exactement 6 chiffres. Veuillez réessayer.")
      }
    }
  }

  def restockMachine(machineId: Int, StockCafeReel: Array[Int], StockSucreReel: Array[Int], StockLaitReel: Array[Int]): Unit = {
    println(s"Réapprovisionnement de la machine ${machineId + 1}")

    // Demander les quantités
    println("Quantité de café à ajouter (g) : ")
    val coffee = scala.io.StdIn.readInt()
    println("Quantité de sucre à ajouter (g) : ")
    val sugar = scala.io.StdIn.readInt()
    println("Quantité de lait à ajouter (L) : ")
    val milk = scala.io.StdIn.readDouble()

    // Mettre à jour
    StockCafeReel(machineId) += coffee
    StockSucreReel(machineId) += sugar
    StockLaitReel(machineId) = StockLaitReel(machineId) + (milk * 1000).toInt

    println(s"Réapprovisionnement réussi pour la machine ${machineId + 1}")
  }

  def main(args: Array[String]): Unit = {
  }

    while (run){
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
            Mode1=true
            Demande = false

            while (Mode1){
              var DemandeId = true
            while (DemandeId) {
              println("Machine sélectionnée (1-5)")
              println(">")
              var machineId = scala.io.StdIn.readLine()
              if (machineId == "1"){
                machineId1=0
                DemandeId=false
              }
              else if (machineId == "2"){
                machineId1=1
                DemandeId=false
              }
              else if (machineId == "3"){
                machineId1=2
                DemandeId=false
              }
              else if (machineId == "4"){
                machineId1=3
                DemandeId=false
              }
              else if (machineId == "5"){
                machineId1=4
                DemandeId=false
              }
              else {
                println("Merci de choisir une machine entre 1 et 5")
              }
            }

              def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {

                var coffeeStocks = 0
                var StockLait = 0
                var StockSucre =0
                var DemandeBoisson = true
            while (DemandeBoisson) {

              prix=0

              println("Sélectionnez votre boisson : :")
              println("1) Expresso - CHF 2.00")
              println("2) Cappuccino - CHF 2.50")
              println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
              println(">")

              boisson = scala.io.StdIn.readLine()

              if (boisson == "1") {
                prix += 2.00
                coffeeStocks += 8
                PrixBase=" CHF 2.00"
                DemandeBoisson = false
              } else if (boisson == "2") {
                prix += 2.50
                coffeeStocks += 6
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
                    coffeeStocks += 6
                    StockLait += 120
                    taille=" (Petit)"
                    PrixBase=" CHF 2.70"
                    DemandeLatte = false
                  } else if (Latte == "2") {
                    prix += 3.20
                    coffeeStocks += 8
                    StockLait += 150
                    taille=" (Moyen)"
                    PrixBase=" CHF 3.20"
                    DemandeLatte = false
                  } else if (Latte == "3") {
                    prix += 3.70
                    coffeeStocks += 12
                    StockLait += 200
                    taille=" (Grand)"
                    PrixBase=" CHF 3.70"
                    DemandeLatte = false
                  } else {
                    println("Sélection invalide, veuillez réessayer.")
                  }
                }
                DemandeBoisson = false
              } else {
                println("Sélection invalide, veuillez réessayer.")
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
                if (StockLait > StockLaitReel(machineId1) && Latte == "2") {
                  println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                  println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
                  println("")
                  false

                } else if (coffeeStocks > StockCafeReel(machineId1) && Latte == "2") {
                  println("Erreur : Quantité de Café insuffisante pour préparer la boisson sélectionnée.")
                  println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
                  println("")
                  false
                } else if (StockLait > StockLaitReel(machineId1) && Latte == "3") {
                  println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                  println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
                  println("")
                  false
                } else if (coffeeStocks > StockCafeReel(machineId1) && Latte == "3") {
                  println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
                  println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
                  println("")
                  false

                } else if (coffeeStocks > StockCafeReel(machineId1) && Latte == "1") {
                  println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  println("")
                  false
                } else if (StockLait > StockLaitReel(machineId1) && Latte == "1") {
                  println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  println("")
                  false
                } else if (StockSucre > StockSucreReel(machineId1) && Latte == "1") {
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  println("")
                  false

                } else if (coffeeStocks > StockCafeReel(machineId1) && (boisson == "1" || boisson == "2")) {
                  println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  println("")
                  false
                } else if (StockLait > StockLaitReel(machineId1) && (boisson == "1" || boisson == "2")) {
                  println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  println("")
                  false
                } else if (StockSucre > StockSucreReel(machineId1) && (boisson == "1" || boisson == "2")) {
                  println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
                  println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                  println("")
                  false
                } else {

                  val caracterepossible = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
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
                StockLaitReel(machineId1) -= StockLait
                StockCafeReel(machineId1) -= coffeeStocks
                StockSucreReel(machineId1) -= StockSucre
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
                Mode1=false
                  StockLait=0
                  StockSucre=0
                  coffeeStocks=0
                true
            }
          }
              serveClient(machineId1, StockCafe, StockSucre, StockLait)}
        }
        else if (mode == "2") {
          println("Mode Admin")
          var DemandeId = true
          while (DemandeId) {
            println("Machine sélectionnée (1-5)")
            println(">")
            val machineId = scala.io.StdIn.readLine()
            if (machineId == "1"){
              machineId1=0
              DemandeId=false
            }
            else if (machineId == "2"){
              machineId1=1
              DemandeId=false
            }
            else if (machineId == "3"){
              machineId1=2
              DemandeId=false
            }
            else if (machineId == "4"){
              machineId1=3
              DemandeId=false
            }
            else if (machineId == "5"){
              machineId1=4
              DemandeId=false
            }
            else {
              println("Merci de choisir une machine entre 1 et 5")
            }
          }
          var Code=true
          while (Code){
           validatePin(machineId1, machinePins)
            println(s"Accès accordé à la Machine ${machineId1 + 1}")
            Code=false
          }

          var option = true
          while(option){
            println("1) Gérer les stocks")
            println("2) Mettre a jour le code PIN")
            println(">")
            val repoption = scala.io.StdIn.readLine()
            if (repoption=="1"){
              option=false
              println("Nivaux de stock actuels :")
              println("Poudre de café : " + StockCafeReel(machineId1) + "g")
              println("Sucre : " + StockSucreReel(machineId1) + "g")
              println("Lait : " + StockLaitReel(machineId1)/1000.0 + "L")

              restockMachine(machineId1, StockCafeReel, StockSucreReel, StockLaitReel)
              println("Les stocks ont été mis à jour avec succès.")
              println("Retour au menu principal...")
              println("")

            }
            else if (repoption=="2"){
              updatePin(machineId1, machinePins)
            }
            else{
              println("Merci de répondre par 1 ou par 2")
            }
          }
        }


        else if (mode == "3") {
          println("Au revoir")
          Demande = false
          run=false
        }}}
    }











