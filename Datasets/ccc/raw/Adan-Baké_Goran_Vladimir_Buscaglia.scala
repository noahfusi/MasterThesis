object Nospresso {

  import scala.collection.mutable.ArrayBuffer
  import scala.io.Source
  import java.io.{File, PrintWriter}

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

  var StockCafeReel = 0
  var StockSucreReel = 0
  var StockLaitReel = 0
  var machineId1 = 0


  case class Machine(id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {
    def addIngredient(ingredient: String, amount: Int): Unit = {
      ingredient.toLowerCase match {
        case "milk" => milk += amount
        case "sugar" => sugar += amount
        case "coffee" => coffee += amount
        case _ => println(s"Ingrédient inconnu : $ingredient")
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      ingredient.toLowerCase match {
        case "milk" => milk -= amount
          true
        case "sugar" => sugar -= amount
          true
        case "coffee" => coffee -= amount
          true
        case _ =>
          println(s"Stock insuffisant ou ingrédient inconnu : $ingredient")
          false
      }
    }

    def changerPincode(nouveauPincode: String): Unit = {
      pincode = nouveauPincode
      println(s"Le pincode a été changé en : $nouveauPincode")
    }
  }

  // Charger les données depuis le CSV
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      val lines = Source.fromFile(filename).getLines().drop(1)
      for (line <- lines) {
        val cols = line.split(",").map(_.trim)
        if (cols.length == 4) {
          val pincode = cols(0)
          val milk = cols(1).toInt
          val sugar = cols(2).toInt
          val coffee = cols(3).toInt
          val id = machines.length + 1
          machines += Machine(id, pincode, milk, sugar, coffee)
        }
      }
    } catch {
      case e: Exception => println(s"Erreur lors de la lecture du fichier CSV")
    }
    machines
  }

  // Sauvegarder dans le CSV
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val writer = new PrintWriter(new File(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        writer.println(s"${machine.pincode},${machine.milk},${machine.sugar},${machine.coffee}")
      }
      writer.close()
    } catch {
      case e: Exception => println(s"Erreur lors de l'écriture du fichier CSV")
    }
  }

  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var essais = 0
    val essaisMax = 3
    var BonCode = false

    val CodeCorrect = machines(machineId).pincode

    while (essais < essaisMax && !BonCode) {
      println("Entrez le code PIN : ")
      val pin = scala.io.StdIn.readLine()

      if (pin == CodeCorrect) {
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

  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    var CodeAccepter = false

    while (!CodeAccepter) {
      println(s"Mise à jour du code PIN pour la Machine ${machineId1 + 1}.")
      println("Entrez un nouveau code PIN à 6 chiffres : ")

      val NouveauCode = scala.io.StdIn.readLine()

      if (NouveauCode.length == 6 && NouveauCode.forall(_.isDigit)) {
        machines(machineId1).changerPincode(NouveauCode)
        println(s"Le code PIN a été mis à jour avec succès.")
        println("Retour au menu principal...")
        println("")
        CodeAccepter = true
      } else {
        println("Le code PIN doit comporter exactement 6 chiffres. Veuillez réessayer.")
      }
    }
  }

  val filename = "machines.csv"
  val machines = loadcsv(filename)

  loadcsv("machines.csv")


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

          // Remettre les valeurs
          machines.find(_.id == machineId1+1) match {
            case Some(machine) => StockLaitReel={machine.milk}
            case None => println(s"Machine avec l'identifiant $machineId1+1 non trouvée")
          }

          machines.find(_.id == machineId1+1) match {
            case Some(machine) => StockCafeReel={machine.coffee}
            case None => println(s"Machine avec l'identifiant $machineId1+1 non trouvée")
          }

          machines.find(_.id == machineId1+1) match {
            case Some(machine) => StockSucreReel={machine.sugar}
            case None => println(s"Machine avec l'identifiant $machineId1+1 non trouvée")
          }

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
            if (StockLait > StockLaitReel && Latte == "2") {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              println("")


            } else if (coffeeStocks > StockCafeReel && Latte == "2") {
              println("Erreur : Quantité de Café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              println("")

            } else if (StockLait > StockLaitReel && Latte == "3") {
              println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              println("")

            } else if (coffeeStocks > StockCafeReel && Latte == "3") {
              println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
              println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
              println("")


            } else if (coffeeStocks > StockCafeReel && Latte == "1") {
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


            } else if (coffeeStocks > StockCafeReel && (boisson == "1" || boisson == "2")) {
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

              //reset des variables

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

              //retrait d'ingrédients

              machines.find(_.id == machineId1+1).foreach { machine =>
                machine.removeIngredient("coffee", coffeeStocks)
              }
              machines.find(_.id == machineId1+1).foreach { machine =>
                machine.removeIngredient("sugar", StockSucre)
              }
              machines.find(_.id == machineId1+1).foreach { machine =>
                machine.removeIngredient("milk", StockLait)
              }

              StockLait=0
              StockSucre=0
              coffeeStocks=0

              savecsv("machines.csv", machines)


            }
          }
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
          validatePin(machineId1, machines)
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
            loadcsv("machines.csv")
            machines.find(_.id == machineId1+1) match {
              case Some(machine) => StockLaitReel={machine.milk}
              case None => println(s"Machine avec l'identifiant $machineId1+1 non trouvée")
            }

            machines.find(_.id == machineId1+1) match {
              case Some(machine) => StockCafeReel={machine.coffee}
              case None => println(s"Machine avec l'identifiant $machineId1+1 non trouvée")
            }

            machines.find(_.id == machineId1+1) match {
              case Some(machine) => StockSucreReel={machine.sugar}
              case None => println(s"Machine avec l'identifiant $machineId1+1 non trouvée")
            }

            println("Nivaux de stock actuels :")
            println("Poudre de café : " + StockCafeReel + "g")
            println("Sucre : " + StockSucreReel + "g")
            println("Lait : " + StockLaitReel/1000.0 + "L")

            println("Combien de café avez vous rajouté")
            val PlusCafe = scala.io.StdIn.readLine().toInt

            println("Combien de sucre avez vous rajouté")
            val PlusSucre = scala.io.StdIn.readLine().toInt

            println("Combien de lait avez vous rajouté")
            val PlusLait = scala.io.StdIn.readLine().toInt


            machines.find(_.id == machineId1+1).foreach { machine =>
              machine.addIngredient("coffee", PlusCafe)}
            machines.find(_.id == machineId1+1).foreach { machine =>
              machine.addIngredient("sugar", PlusSucre)}
            machines.find(_.id == machineId1+1).foreach { machine =>
              machine.addIngredient("milk", PlusLait)}

            savecsv("machines.csv", machines)


            println("Les stocks ont été mis à jour avec succès.")
            println("Retour au menu principal...")
            println("")
          }
          else if (repoption=="2"){
            updatePin(0, machines)
          }
          else{
            println("Merci de répondre par 1 ou par 2")
          }
        }
      }


      else if (mode == "3") {

        savecsv("machines.csv", machines)
        println("Au revoir")
        Demande = false
        run=false
      }}}
}

