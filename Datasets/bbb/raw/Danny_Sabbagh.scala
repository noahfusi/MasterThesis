import scala.io.StdIn._
import scala.util.Random

object Main {
  val nbMachines = 5
  val coffeeStocks = List.tabulate(nbMachines)(x => 50).toArray
  val sugarStocks = List.tabulate(nbMachines)(x => 30).toArray
  val milkStocks = List.tabulate(nbMachines)(x => 500).toArray
  val machinePin = List.tabulate(nbMachines)(x => "434343").toArray
  def main(args: Array[String]): Unit = {
    while (true) {
      var choix = 0
      while (choix != 1 && choix != 2 && choix != 3) {
        println("Nospresso Cafe \nVeuillez sélectionner votre mode : \n1) Client\n2) Admin\n3) Quitter")
        choix = readInt()
      }
      if (choix == 3) return

      var choixmachine = 0
      while (choixmachine != 1 && choixmachine != 2 && choixmachine != 3 && choixmachine != 4 && choixmachine != 5) {
        print("Machine sélectionnée (1-" + nbMachines +") > ")
        choixmachine = readInt()
      }

      if (choix == 1){
        serveClient(choixmachine-1, coffeeStocks, sugarStocks, milkStocks)
      } else if (choix == 2) {
        println("Entrez le code PIN : ")

        var nbressais =  0
        while(!validatePin(machineId = choixmachine-1, machinePins = machinePin)){
          nbressais += 1
          if (nbressais < 3){
            println("Code PIN incorrect. " + (3 - nbressais) + " tentatives restantes")
          } else if (nbressais == 2){
            println("Code PIN incorrect. " + (3 - nbressais) + " tentative restante")
          }else{
            println("Trop de tentatives échouées. Fin du programme")
            return
          }
        }
        println("Accès accordé.")
        var choixAdmin = 0
        while (choixAdmin !=1 && choixAdmin!= 2){
          println("Choisissez votre mode: \n 1) Modification du code PIN \n 2) Recharge des stocks")
          choixAdmin = readInt()
        }
        if (choixAdmin == 1){
          updatePin(choixmachine-1, machinePin)
        }else{
          restockMachine(choixmachine-1, coffeeStocks,sugarStocks, milkStocks)
        }

      } else if (choix == 3) {
        return
      } else {
        print("Valeur non autorisée")
      }
      print("\n")
    }
  }
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean ={
    print("> ")
    var PINattempt = readInt()
    return PINattempt == machinePins(machineId).toInt
  }
  def updatePin(machineId: Int, machinePins: Array[String]): Unit ={
    println("Mise à jour du code PIN pour la Machine " + machineId + ".")
    print("Entrez un nouveau code PIN à 6 chiffres > ")
    var newPIN = readInt()
    while (newPIN.toString.length != 6){
      print("Entrez un nouveau code PIN à 6 chiffres > ")
      newPIN = readInt()
    }
    machinePins(machineId) = newPIN.toString
    println("Le code PIN à été mis à jour avec succès. \nRetour au menu principal... ")
  }
  def serveClient(machineId: Int, coffeeStocks: Array[Int],
                  sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean ={
    val boisson = Array("Expresso", "Cappuccino", "Latte (Petit)", "Latte (Moyen)", "Latte (Grand)")
    val tableausucre = Array("Sans sucre", "Peu (5g)", "Moyen (10g)", "Beacoup (15g)")
    val tableaulait = Array("Oui", "Non")
    val tableaupoudre = Array(8, 6, 6, 8, 12)
    val tableauqtesucre = Array(0, 5, 10, 15)
    val tableauqtelait = Array(0, 100, 120, 150, 200)

    val prixboisson = Array(2.00, 2.50, 2.70, 3.20, 3.70)
    val prixsucre = Array(0.00, 0.10, 0.20, 0.30)
    val prixlait = 0.05
    var choixboisson = 0
    while (choixboisson != 1 && choixboisson != 2 && choixboisson != 3) {
      println("\nVeuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      choixboisson = readInt()
    }
    var choixtaille = 0
    if (choixboisson == 3) {
      while (choixtaille != 1 && choixtaille != 2 && choixtaille != 3) {
        println("\nVeuillez sélectionner la taille de votre latte: \n1) Petit \n2) Moyen \n3) Grand")
        choixtaille = readInt()
      }
    }
    var choixsucre = 0
    while (choixsucre != 1 && choixsucre != 2 && choixsucre != 3 && choixsucre != 4) {
      println("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
      choixsucre = readInt()
    }
    var choixlait = 0
    var choixdose = 0
    if (choixboisson != 1) {
      while (choixlait != 1 && choixlait != 2) {
        println("\nSouhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte) \n1) Oui\n2) Non")
        choixlait = readInt()
      }
      if (choixlait == 1) {
        while (choixdose != 1 && choixdose != 2 && choixdose != 3) {
          println("\nCombien de dose ?")
          choixdose = readInt()
        }
      }
    }

    if (choixboisson == 3) {
      println("\nBoisson sélectionnée : " + boisson(choixboisson - 1 + choixtaille - 1))

    } else {
      println("\nBoisson sélectionnée : " + boisson(choixboisson - 1))

    }
    println("Niveau de sucre : " + tableausucre(choixsucre - 1))
    if (choixboisson != 1) {
      println("Lait supplémentaire: " + tableaulait(choixlait - 1))

    }
    var poudreqte = 0
    if (choixboisson == 3) {
      poudreqte = tableaupoudre(choixboisson - 1 + choixtaille - 1)

    } else {
      poudreqte = tableaupoudre(choixboisson - 1)

    }
    var laitqte = 0
    if (choixboisson == 2) {
      laitqte = tableauqtelait(choixboisson - 1)
    }
    if (choixboisson == 3) {
      laitqte = tableauqtelait(choixboisson - 1 + choixtaille - 1)
    }
    if (choixlait == 1) {
      laitqte += choixdose * 5
    }
    var sucreqte = tableauqtesucre(choixsucre - 1)
    if (poudreqte > coffeeStocks(machineId)) {
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
      return false
    }
    if (laitqte > milkStocks(machineId)) {
      println("Erreur : Quantité́ de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.")
      return false
    }
    if (sucreqte > sugarStocks(machineId)) {
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou vérifier les stocks en mode Admin.")
      return false
    }
    if (sucreqte <= sugarStocks(machineId) && laitqte <= milkStocks(machineId) && poudreqte <= coffeeStocks(machineId)) {
      sugarStocks(machineId) -= sucreqte
      milkStocks(machineId) -= laitqte
      coffeeStocks(machineId) -= poudreqte
      var prixtexte = ""
      var prix = 0.00
      if (choixboisson == 3) {
        prixtexte = "CHF " + "%.2f".formatLocal(java.util.Locale.US, prixboisson(choixboisson - 1 + choixtaille - 1))
        prix = prixboisson(choixboisson - 1 + choixtaille - 1)
      } else {
        prixtexte = "CHF " + "%.2f".formatLocal(java.util.Locale.US, prixboisson(choixboisson - 1))
        prix = prixboisson(choixboisson - 1)
      }

      if (choixsucre != 1) {
        prixtexte += " + CHF " + "%.2f".formatLocal(java.util.Locale.US, prixsucre(choixsucre - 1))
        prix += prixsucre(choixsucre - 1)

      }
      if (choixlait == 1) {
        prixtexte += " + CHF " + "%.2f".formatLocal(java.util.Locale.US, (choixdose * prixlait))
        prix += choixdose * prixlait

      }
      prixtexte += " = CHF " + "%.2f".formatLocal(java.util.Locale.US, prix)
      println("Prix total : " + prixtexte)
      println("\nVeuillez payer en utilisant Twint.")
      println("Votre code de paiement est : " + Random.alphanumeric.take(5).mkString("").toUpperCase())
      println("(En attente de paiement...)\n")
      Thread.sleep(3000)
      println("Paiement confirmé.\nPréparation de votre boisson...")
      println("[...]")
      Thread.sleep(5000)
      println("Votre " + boisson(choixboisson - 1).split(" ")(0) + " est prêt ! Bonne dégustation !")
    }
    return true
  }
  def restockMachine(machineId: Int, coffeeStocks: Array[Int],
                     sugarStocks: Array[Int], milkStocks: Array[Int]): Unit ={
    println("Accès autorisé.\n\nStocks :")
    println("  Poudre de café : " + coffeeStocks(machineId) + "g")
    println("  Lait           : " + milkStocks(machineId) / 1000.0 + "L")
    println("  Sucre          : " + sugarStocks(machineId) + "g")
    println("\nRéapprovisionnement des stocks...")

    println("Ajout :")
    print("  Poudre de café: ")
    val ajoutPoudre = readInt()
    print("  Lait          : ")
    val ajoutLait = readDouble()
    print("  Sucre         : ")
    val ajoutSucre = readInt()

    coffeeStocks(machineId) += ajoutPoudre
    milkStocks(machineId) += (ajoutLait * 1000).toInt
    sugarStocks(machineId) += ajoutSucre

    println("Niveaux de stock mis à jour.")
    println("Retour au menu principal...")
  }
}