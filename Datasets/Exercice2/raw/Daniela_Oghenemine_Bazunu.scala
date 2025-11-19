import scala.io.StdIn
import scala.util.Random

object Nospresso {

  val nbMachines: Int = 5
  val coffeeStocks: Array[Int] = Array.fill(nbMachines)(50)
  val sugarStocks: Array[Int] = Array.fill(nbMachines)(30)
  val milkStocks: Array[Int] = Array.fill(nbMachines)(500)
  val machinePins: Array[String] = Array.fill(nbMachines)("434343")



  val basePrices = Map(

    "espresso" -> 2.0,

    "cappuccino" -> 3.0,

    "petitLatte" -> 2.5,

    "moyenLatte" -> 3.5,

    "grandLatte" -> 4.0

  )

  def generateTwintCode(): String = {
    Random.alphanumeric.filter(_.isLetterOrDigit).take(5).mkString
  }

  def calculerPrixFinal(boisson: String, niveauSucre: Int, dose: Int): Double = {

    val prixSucre =

      if (niveauSucre == 1) 0.10

      else if (niveauSucre == 2) 0.20

      else if (niveauSucre == 3) 0.30

      else 0.0



    val prixLaitSupp = dose * 0.05



    if (basePrices.contains(boisson)) {

      val prixDeBase = basePrices(boisson)

      prixDeBase + prixSucre + prixLaitSupp

    } else {

      println("Boisson inconnue. Veuillez choisir une boisson valide.")

      0.0

    }

  }

  def effectuerPaiement(prixfinal: Double): Unit = {
    val codeTwint = generateTwintCode()
    println(s"Veuillez payer CHF $prixfinal via Twint.")
    println(s"Votre code de paiement est : $codeTwint")
    Thread.sleep(3000)
    println("Paiement confirmé. Merci !")
  }




  def clientMode(): Unit = {
    println("choissisez une machine: 0,1,2,3,4):")
    val machineId = StdIn.readLine().toInt
    if (machineId >= 0 && machineId < nbMachines) {
      choixdeboisson(machineId, coffeeStocks, sugarStocks, milkStocks)
    } else {
      println("invalide.")
    }
  }

  def adminMode(): Unit = {
    println("Entrez l'identifiant de la machine (0 à 4):")
    val machineId = StdIn.readLine().toInt
    if (machineId >= 0 && machineId < nbMachines) {
      if (validatePin(machineId, machinePins)) {
        println("1) Réapprovisionner les stocks")
        println("2) Mettre à jour le code PIN")
        println(">")
        val choice = StdIn.readLine()

        if (choice == "1") restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
        else if (choice == "2") updatePin(machineId, machinePins)
        else println("Choix invalide.")
      }
    } else {
      println("Identifiant de machine invalide.")
    }
  }
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println(s"Réapprovisionnement de la machine $machineId")
    println("Entrez la quantité de café à ajouter (en grammes):")
    val coffeeAdded = StdIn.readLine().toInt
    println("Entrez la quantité de sucre à ajouter (en grammes):")
    val sugarAdded = StdIn.readLine().toInt
    println("Entrez la quantité de lait à ajouter (en millilitres):")
    val milkAdded = StdIn.readLine().toInt

    coffeeStocks(machineId) += coffeeAdded
    sugarStocks(machineId) += sugarAdded
    milkStocks(machineId) += milkAdded

    println(s"Stocks mis à jour pour la machine $machineId:")
    println(s"Café: ${coffeeStocks(machineId)}g, Sucre: ${sugarStocks(machineId)}g, Lait: ${milkStocks(machineId)}ml")
  }



  def main(args: Array[String]): Unit = {
    var running = true
    while (running) {
      println("Bienvenue sur Nospresso !")
      println("1) Mode Client")
      println("2) Mode Admin")
      println("3) Quitter")
      println(">")
      val choice = StdIn.readLine()

      if (choice == "1") clientMode()
      else if (choice == "2") adminMode()
      else if (choice == "3") running = false
      else println("Choix invalide. Veuillez réessayer.")
    }
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var attempts = 3
    while (attempts > 0) {
      println(s"Entrez le code PIN pour la machine $machineId (tentatives restantes: $attempts):")
      val inputPin = StdIn.readLine()
      if (inputPin == machinePins(machineId)) {
        println("Accès autorisé.")
        return true
      } else {
        attempts -= 1
        println("Code PIN incorrect.")
      }
    }
    println("Nombre de tentatives dépassé. Fin du programme.")
    System.exit(1)
    false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Entrez un nouveau code PIN à 6 chiffres:")
    var validPin = false
    while (!validPin) {
      val newPin = StdIn.readLine()
      if (newPin.matches("\\d{6}")) {
        machinePins(machineId) = newPin
        println(s"Le code PIN de la machine $machineId a été mis à jour.")
        validPin = true
      } else {
        println("Le code PIN doit comporter exactement 6 chiffres. Veuillez réessayer:")
      }
    }
  }

  def choixdeboisson(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    println("veuillez choisir une boisson")
    println("1.1) Expresso - CHF 2.00")
    println("1.2) cppucino- CHF 2.50")
    println("1.3) latte => 1.3.1) CHF 2.70 (petit); 1.3.2) CHF 3.20 (moyen); 1.3.3) CHF 3.70 (grand)")
    println ("1.4) annuler")
    println ( ">")
    val choice = StdIn.readLine()

    if (choice == "1.1"){
      println("vous avez choisi un expresso")
    } else if (choice=="1.2"){
      println("vous avez choisi un capuccino")
    }else if (choice=="1.3.1"){
      println("vous avez choisi un petit latte")
    } else if (choice=="1.3.2"){
        println("vous avez choisi un latte moyen")
      } else if (choice=="1.3.3"){
        println("vous avez choisi: grand")
      }




    val coffeeRequired = if (choice == "1.1") 8
    else if (choice == "1.2") 6
    else if (choice == "1.3.1")6
    else if (choice == "1.3.2")8
    else if (choice=="1.3.3")12

    else if (choice == "4")
      return false else {
      println("Choix invalide.")
      return false
    }

    val milkRequired = if (choice == "1.1") 0
    else if (choice == "1.2") 100
    else if (choice == "1.3.1") 120
    else if (choice=="1.3.2") 150
    else if (choice=="1.3.3") 200
    else 0


    if (coffeeStocks(machineId) >= coffeeRequired && milkStocks(machineId) >= milkRequired) {
      coffeeStocks(machineId) -= coffeeRequired
      milkStocks(machineId) -= milkRequired
      println(s"vous avez choisi ${choice}")
      Thread.sleep(2000)
      println("souhaites-vous rajouter du sucre?")
      Thread.sleep(2000)
      println("1) sans sucre")
      println("2) peu- 5g- CHF 0.10")
      println("3) moyen- 10g- CHF 0.20")
      println("4) beacoup- 15g- CHF 0.30")
      print(">")

      val personalisation= StdIn.readLine()

      val sugarRequired: Int = if (personalisation=="1") 0
      else if (personalisation=="2") 5
      else if (personalisation=="3") 10
      else if (personalisation=="4") 15
      else 0

      var niveaudesucre = 0

      if (personalisation=="1"){
        niveaudesucre= 0
      } else if (personalisation=="2"){
        niveaudesucre= 1
      }else if (personalisation=="3"){
        niveaudesucre= 2
      }else if (personalisation=="4"){
        niveaudesucre= 3
      }

      if (sugarStocks(machineId) >= sugarRequired ) {
        sugarStocks(machineId) -= sugarRequired
        println(s"vous avez choisi ${personalisation}")
        Thread.sleep(2000)
        println("souhaitez-vous ajouter du lait supplémentaire?")
        Thread.sleep(2000)
          println("combien de dose?")
          Thread.sleep(2000)
          println("0")
          println("1")
          println("2")
          println("3")
          println(">")
          val dose= StdIn.readLine().toInt
          if (dose==0){
            println("patienter...")
            if (choice=="1.1") {
            val prixfinal=calculerPrixFinal("espresso", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            } else if (choice=="1.2"){
             val prixfinal=calculerPrixFinal("capuccino", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
              }else if (choice=="1.3.1"){
              val prixfinal=calculerPrixFinal("petitLatte", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            }else if (choice=="1.3.2"){
              val prixfinal=calculerPrixFinal("moyenLatte", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            }else if (choice=="1.3.3"){
              val prixfinal= calculerPrixFinal("grandLatte", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            }

          } else if (dose==1){
            println("patienter...")
            if (choice=="1.1") {
              val prixfinal=calculerPrixFinal("espresso", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            } else if (choice=="1.2"){
              val prixfinal=calculerPrixFinal("capuccino", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            }else if (choice=="1.3.1"){
              val prixfinal=calculerPrixFinal("petitLatte", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            }else if (choice=="1.3.2"){
              val prixfinal=calculerPrixFinal("moyenLatte", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            }else if (choice=="1.3.3"){
              val prixfinal=calculerPrixFinal("grandLatte", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            }

          } else if (dose==2){
            println("patienter...")
            if (choice=="1.1") {
              val prixfinal=calculerPrixFinal("espresso", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            } else if (choice=="1.2"){
              val prixfinal=calculerPrixFinal("capuccino", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            }else if (choice=="1.3.1"){
              val prixfinal= calculerPrixFinal("petitLatte", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            }else if (choice=="1.3.2"){
              val prixfinal=calculerPrixFinal("moyenLatte", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            }else if (choice=="1.3.3"){
              val prixfinal=calculerPrixFinal("grandLatte", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            }

          } else if(dose==3){
            println("patienter...")
            if (choice=="1.1") {
              val prixfinal=calculerPrixFinal("espresso", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            } else if (choice=="1.2"){
              val prixfinal=calculerPrixFinal("capuccino", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            }else if (choice=="1.3.1"){
              val prixfinal=calculerPrixFinal("petitLatte", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            }else if (choice=="1.3.2"){
              val prixfinal=calculerPrixFinal("moyenLatte", niveaudesucre, dose)
                effectuerPaiement(prixfinal)
            }else if (choice=="1.3.3"){
              val prixfinal=calculerPrixFinal("grandLatte", niveaudesucre, dose)
              effectuerPaiement(prixfinal)
            }

          }
        }







    }

      println("Profitez de votre boisson!")
      true
  }
}




