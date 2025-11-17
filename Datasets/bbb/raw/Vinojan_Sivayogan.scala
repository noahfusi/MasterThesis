import io.StdIn._
import util.Random

object Main {
  def main(args: Array[String]): Unit = {
    //Décalration des variables
    val nbMachines = 5
    var machineId = -1
    val defaultPIN = "434343"

    var enMarche = true

    val coffeeStocks = Array.fill(nbMachines)(50)
    val sugarStocks = Array.fill(nbMachines)(30)
    val milkStocks = Array.fill(nbMachines)(500)
    val machinePins = Array.fill(nbMachines)(defaultPIN)

    //Début du programme
    while (enMarche){
      var mode = 0

      //Choix du mode
      println("\tNospresso Café")
      println("Veuillez sélectionner votre mode :")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")

      mode = readLine("> ").toInt

      if (mode == 1){
        //Mode Client
        //Choix de la machine
        machineId = readLine(f"\nMachine sélectionnée (1-$nbMachines) > ").toInt - 1
        if (machineId >= 0 && machineId <= nbMachines - 1){
          serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
        }else{
          println("\nEntrée invalide")
        }
      }else if (mode == 2) {
        //Mode Admin
        //Choix de la machine
        machineId = readLine(f"\nMachine sélectionnée (1-$nbMachines) > ").toInt - 1
        if (machineId >= 0 && machineId <= nbMachines - 1){
          if(validatePin(machineId, machinePins)){
            println("\n1) Réapprovisionner les stocks")
            println("2) Mettre à jour le code PIN")
            var choixAdmin = readLine("> ").toInt

            if(choixAdmin == 1){
              restockMachine(machineId, coffeeStocks, sugarStocks, milkStocks)
            }else if(choixAdmin == 2){
              updatePin(machineId, machinePins)
            }else{
              println("\nEntrée invalide")
            }
          }else{
            println("\nTrop de tentatives échouées. Fin du programme.")
            enMarche = false
          }
        }else{
          println("\nEntrée invalide")
        }
      }else if (mode == 3){
        enMarche = false
        println("\nFin du programme")
      }else{
        println("\nEntrée invalide")
      }
    }
  }

  //Valider le code PIN pour la machine sélectionnée
  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var tentative = 3

    while (tentative > 0){
      println("Entrez le code PIN :")
      var Pin = readLine("> ")
      if (machinePins(machineId).equals(Pin)){
        println(f"Accès accordé à la Machine ${machineId + 1}.")
        return true
      }else{
        tentative -= 1
        println(f"Code PIN incorrect. $tentative tentatives restantes.")
      }
    }
    return false
  }

  //Mettre à jour le code PIN de la machine sélectionnée
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    var nouveauPin = ""
    println(f"Mise à jour du code PIN pour la machine ${machineId+1}.")

    do{
      nouveauPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    }while(nouveauPin.length != 6 || !nouveauPin.forall(_.isDigit))
    machinePins(machineId) = nouveauPin

    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }

  //Traiter une transaction client pour la machine sélectionnée
  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean ={
    val prixExpresso = 2.00
    val prixCappuccino = 2.50
    val prixLatteS = 2.70
    val prixLatteM = 3.20
    val prixLatteL = 3.70
    val prixSucre = 0.10

    val erreur = "\nEntrée invalide"

    var boisson = 0
    var nomBoisson = ""
    var tailleLatte = 0
    var nomTailleLatte = ""
    var nivSucre = 0
    var nomNivSucre = ""
    var nivLait = 0
    var nomNivLait = ""
    var cafeRequis = 0
    var sucreRequis = 0
    var laitRequis = 0
    var prix = 0.00

    //Choix de la boisson
    println("\nVeuillez sélectionner votre boisson :")
    println(f"1) Expresso - CHF $prixExpresso%.2f")
    println(f"2) Cappuccino - CHF $prixCappuccino%.2f")
    println(f"3) Latte - CHF $prixLatteS%.2f (Petit), CHF $prixLatteM%.2f (Moyen), CHF $prixLatteL%.2f (Grand)")

    boisson = readLine("> ").toInt

    if (boisson == 1) {
      nomBoisson = "Expresso"
      prix += prixExpresso
      cafeRequis = 8
    }else if (boisson == 2) {
      nomBoisson = "Cappuccino"
      prix += 2.50
      cafeRequis = 6
      laitRequis = 100
    }else if (boisson == 3) {
      println("\nVeuillez choisir la taille du Latte : ")
      println("1) Petit")
      println("2) Moyen")
      println("3) Grand")

      tailleLatte = readLine("> ").toInt
      nomBoisson = "Latte"
      if (tailleLatte == 1) {
        prix += 2.70
        cafeRequis = 6
        laitRequis = 120
        nomTailleLatte = "(Petit)"
      }else if (tailleLatte == 2) {
        prix += 3.20
        cafeRequis = 8
        laitRequis = 150
        nomTailleLatte = "(Moyen)"
      }else if (tailleLatte == 3) {
        prix += 3.70
        cafeRequis = 12
        laitRequis = 200
        nomTailleLatte = "(Grand)"
      }else {
        println(erreur)
        return false
      }
    }else{
      println(erreur)
      return false
    }

    //Choix taux de sucre
    println("\nSouhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println(f"2) Peu (5g) - CHF $prixSucre%.2f")
    println(f"3) Moyen (10g) - CHF ${prixSucre * 2}%.2f")
    println(f"4) Beaucoup (15g) - CHF ${prixSucre * 3}%.2f")

    nivSucre = readLine("> ").toInt

    if (nivSucre == 1){
      nomNivSucre = "Sans sucre"
    }else if (nivSucre == 2){
      nomNivSucre = "Peu (5g)"
      sucreRequis = 5
      prix += prixSucre
    }else if (nivSucre == 3){
      nomNivSucre = "Moyen (10g)"
      sucreRequis = 10
      prix += prixSucre*2
    }else if (nivSucre == 4){
      nomNivSucre = "Beaucoup (15g)"
      sucreRequis = 15
      prix += prixSucre*3
    }else{
      println(erreur)
      return false
    }

    //Choix taux de lait pour Cappuccino et Latte
    if (boisson == 2 || boisson == 3) {
      println("\nSouhaitez-vous ajouter du lait en suppléement ?")
      println("(Disponible uniquement pour Cappuccino et Latte)")
      println("1) Oui")
      println("2) Non")

      var LaitSuppl = readLine("> ").toInt

      //Choix nombre de dose
      if (LaitSuppl == 1) {
        nomNivLait = "Oui"
        println("\nCombien de dose (1-3) ?")
        nivLait += readLine("> ").toInt
        laitRequis += nivLait * 50
        prix += nivLait * 0.05
      }else if (LaitSuppl == 2){
        nomNivLait = "Non"
      }else{
        println(erreur)
        return false
      }
    }

    //Vérification des stocks
    if (coffeeStocks(machineId) >= cafeRequis && sugarStocks(machineId) >= sucreRequis && milkStocks(machineId) >= laitRequis){
      println(f"\nBoisson sélectionnée : $nomBoisson")
      println(f"Niveau de sucre : $nomNivSucre")
      println(f"Lait supplémentaire: $nomNivLait")
      println(f"Prix total : $prix%.2f")

      //Paiement
      val twint = Random.alphanumeric.take(5).mkString("")
      println("\nVeuillez payer en utilisant Twint.")
      println(f"Votre code de paiement est : $twint")
      println(f"(En attente de validation du paiement...)")

      //Pause de 3 secondes
      Thread.sleep(3000)
      println("\nMerci ! Votre paiement a été accepté.")

      //Mise à jour des stocks
      sugarStocks(machineId) -= sucreRequis
      coffeeStocks(machineId) -= cafeRequis
      milkStocks(machineId) -= laitRequis

      println("\nPréparation de votre boisson...")
      println("[...]")

      //Pause de 5 secondes
      Thread.sleep(5000)
      println(f"Votre $nomBoisson est prêt ! Bonne dégustation !")
      return true
    }else {
      if (coffeeStocks(machineId) < cafeRequis){
        println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
      }else if (sugarStocks(machineId) < sucreRequis){
        println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
      }else if (milkStocks(machineId) < laitRequis){
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
      }
      println("Veuillez choisir une taille plus petite, essayez une autre boisson ou choisissez une autre machine.")
      return false
    }
  }

  //Réapprovisionner les ingrédients pour la machine sélectionnée
  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    //Affichage des stocks
    println("\nNiveaux de stock actuels :")
    println("\tPoudre de café\t:\t" + coffeeStocks(machineId) + "g")
    println("\tSucre\t\t\t:\t" + sugarStocks(machineId) + "g")
    println("\tLait\t\t\t:\t" + milkStocks(machineId)/1000 + "L")

    //Réapprovisionnement
    println("\nRéapprovisionnement des stocks ...:")
    println("Ajout :")
    print("\tPoudre de café\t:\t")
    coffeeStocks(machineId) += readInt()
    print("\tSucre\t\t\t:\t")
    sugarStocks(machineId) += readInt()
    print("\tLait\t\t\t:\t")
    milkStocks(machineId) += (readFloat()*1000).toInt

    //Retour au Menu
    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }
}