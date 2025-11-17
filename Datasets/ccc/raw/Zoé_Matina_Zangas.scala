import java.io.{FileWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.StdIn._
import scala.util.Random


object Main {

  // Etape 1: déclaration de la classe
  class Machine(val Id: Int, var pincode: String, var milk: Int, var sugar: Int, var coffee: Int) {

    // 1.1) Methode pour ajouter des ingredients
    def addIngredient(ingredient: String, amount: Int): Unit = {
      // utilisation de la methode remove mais en passant "amount" en nombre négatif (rend le code plus concis)
      removeIngredient(ingredient, amount * -1)
    }

    // 1.2) Methode pour reduire des ingredients seulement si le stock est suffisant
    def removeIngredient(ingredient: String, amount: Int): Boolean = {

      if (ingredient == "lait" && milk >= amount) {
        milk -= amount
        return true
      }
      if (ingredient == "sucre" && sugar >= amount) {
        sugar -= amount
        return true
      }
      if (ingredient == "cafe" && coffee >= amount) {
        coffee -= amount
        return true
      }
      false
    }
  }

  // Etape 2: déclaration des méthodes
  // 2.1) Méthode pour le code PIN en mode Admin: limitation à 3 tentatives
  def validatePin(machine: Machine): Boolean = {

    val tentatives = 3
    for (i <- 1 to tentatives) {
      println("Entrez le code PIN : ")
      println(">")
      val entreecodePin = readLine()

      if (entreecodePin == machine.pincode) {
        println("Accès accordé à la machine " + machine.Id )
        return true
      }
      else {
        println("Code PIN incorrect. " + (tentatives - i) + " tentatives restantes.")
      }
    }
    println()
    println("Trop de tentatives échouées. Fin du programme.")
    false
  }

  // 2.2) Méthode pour la modification du code PIN en mode Admin
  def updatePin(machine: Machine): Unit = {

    println("Mise à jour du code PIN pour la machine " + machine.Id)
    println("Veuillez entrer un nouveau code PIN à 6 chiffres.")
    println(">")
    var nouveaucodePin = readLine()

    // verification que l'entrée est valide (pour le code pin)
    while (nouveaucodePin.length != 6 || !nouveaucodePin.forall(_.isDigit)) {
      println("Nouveau code PIN invalide. Veuillez entrer un nouveau code PIN à 6 chiffres")
      println(">")
      nouveaucodePin = readLine()
    }
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
    machine.pincode = nouveaucodePin
  }

  // 2.3) Méthode pour le mode client
  def serveClient(machine: Machine): Boolean = {

    // variables pour les boissons, la personnalisation et l'affichage
    var choixboisson = 0.0
    var choixsucre = 0
    var choixlait = 0
    var choixdoselait = 0
    var boissonselectionnee1 = "" // pour le récapitulatif ( latte: ajout entre parenthèses de la taille)
    var boissonselectionnee = "" //  pour la phrase après paiement ( latte: pas de précision de la taille)
    var sucreselectionne = ""
    var laitselectionne = ""

    // variables pour les prix
    var prixboisson = 0.00
    var prixtotal = 0.00
    var prixsucre = 0.00
    var prixlait = 0.00

    // variables pour la gestion des stock
    var caferequis = 0
    var sucrerequis = 0
    var laitrequis = 0
    var stocksuffisant = 1

    // variables pour le code Twint
    val longeurcodeTwint = 5
    val alphanumeric = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    var codeTwint = ""

    // réinitialisation des prix (à 0) pour chaque transaction
    prixboisson = 0.0
    prixlait = 0.0
    prixsucre = 0.0
    stocksuffisant = 1

    // Choix de la boisson
    println("Veuillez sélectionner votre boisson:")
    println("1) Expresso - CHF 2.00")
    println("2) Capuccino - CHF 2.50")
    println("Latte:  3.1) Petit - CHF 2.70    3.2) Moyen - CHF 3.20    3.3) Grand - CHF 3.70")
    println(">")
    choixboisson = readDouble()

    // Vérification que l'entrée est valide (pour le choix de la boisson)
    while (choixboisson != 1 && choixboisson != 2 && choixboisson != 3.1 && choixboisson != 3.2 && choixboisson != 3.3) {
      println("Veuillez sélectionner une entrée valide")
      println(">")
      choixboisson = readDouble()
    }

    // changement des différentes variables selon les boissons pour faciliter les calculs et affichages
    if (choixboisson == 1) {
      caferequis = 8
      laitrequis = 0
      boissonselectionnee1 = "Expresso"
      boissonselectionnee = boissonselectionnee1
      prixboisson = 2.00
    }
    else if (choixboisson == 2) {
      caferequis = 6
      laitrequis = 100
      boissonselectionnee1 = "Capuccino"
      boissonselectionnee = boissonselectionnee1
      prixboisson = 2.50
    }
    else if (choixboisson == 3.1) {
      caferequis = 6
      laitrequis = 120
      boissonselectionnee1 = "Latte (Petit)"
      boissonselectionnee = "Latte"
      prixboisson = 2.70
    }
    else if (choixboisson == 3.2) {
      caferequis = 8
      laitrequis = 150
      boissonselectionnee1 = "Latte (Moyen)"
      boissonselectionnee = "Latte"
      prixboisson = 3.20
    }
    else {
      caferequis = 12
      laitrequis = 200
      boissonselectionnee1 = "Latte (Grand)"
      boissonselectionnee = "Latte"
      prixboisson = 3.70
    }

    // Personalisation de la boisson: choix du sucre
    println("Souhaitez vous ajouter du sucre?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    println(">")
    choixsucre = readInt()

    // vérification que l'entrée est valide (pour le choix du sucre)
    while (choixsucre != 1 && choixsucre != 2 && choixsucre != 3 && choixsucre != 4) {
      println("Veuillez sélectionner une entrée valide")
      println(">")
      choixsucre = readInt()
    }

    // changement des différentes variables pour faciliter les calculs et affichages
    sucrerequis = 5 * (choixsucre - 1) // formule génlérale qui englobe les 3 cas possibles
    prixsucre = 0.10 * (choixsucre - 1)

    if (choixsucre == 1) {
      sucreselectionne = "Sans sucre"
    }
    else if (choixsucre == 2) {
      sucreselectionne = "Peu (5g)"
    }
    else if (choixsucre == 3) {
      sucreselectionne = "Moyen (10g)"
    }
    else {
      sucreselectionne = "Beaucoup (15g)"
    }

    // personalisation de la boisson : choix du lait (que pour le cappuccino et latte)
    if ((choixboisson == 2) || (choixboisson == 3.1) || (choixboisson == 3.2) || (choixboisson == 3.3)) {
      println("Souhaitez vous ajouter du lait en supplément ?")
      println("1) oui")
      println("2) non")
      println(">")
      choixlait = readInt()
      prixtotal = prixboisson

      // vérfication que l'entrée est valide (pour le choix du lait)
      while (choixlait != 1 && choixlait != 2) {
        println("Veuillez sélectionner une entrée valide")
        println(">")
        choixlait = readInt()
      }

      // Personalisation de la boisson: choix du nombre de dose de lait (seulement si l'option 1 (Oui) est selectionée)
      // Changement des differentes variables pour faciliter les calculs et affichages
      if (choixlait == 1) {
        laitselectionne = "Oui"
        println("Combien de dose souhaitez vous ? Vous pouvez ajouter jusqu'à 3 dose maximum. 50 ml de lait et - CHF 0.05 par dose")
        println(">")
        choixdoselait = readInt()

        // vérification que l'entrée est valide (pour le choix du nombre de dose de lait)
        while (choixdoselait != 1 && choixdoselait != 2 && choixdoselait != 3) {
          println("Veuillez sélectionner une entrée valide")
          println(">")
          choixdoselait = readInt()
        }

        laitrequis += choixdoselait * 50 // formule générale qui englobe les trois cas possibles
        prixlait = choixdoselait * 0.050
      }

      if (choixlait == 2) {
        laitselectionne = "Non"
        prixlait = 0.0
        choixdoselait = 0
      }
    }

    // Affichage du récapitulatif sans les prix et selon le choix de boisson
    println("Boisson sélectionnée : " + boissonselectionnee1)
    print("Niveau de sucre : " + sucreselectionne)

    if ((choixboisson == 2) || (choixboisson == 3.1) || (choixboisson == 3.2) || (choixboisson == 3.3)) {
      println()
      println("Lait supplémentaire : " + laitselectionne)
    }

    // appel de la méthode removeIngredient pour la validation et reduction du stock selon la boisson
    if (!machine.removeIngredient("cafe", caferequis)) {
      println()
      println("Erreur : quantité de poudre de café insuffisante pour préparer la boisson séléctionnée.")
      if ((choixboisson == 1) || (choixboisson == 2) || (choixboisson == 3.1)) {
        println("Veuillez choisir une autre machine ou verifier les stock en mode Admin.")
      }
      else {
        println("Veuillez choisir une taille plus petite, choisir une autre machine ou verifier les stocks en mode Admin.")
      }
      println()
      return false
    }

    if (!machine.removeIngredient("sucre", sucrerequis)) {
      println()
      println("Erreur: quantité de sucre insuffisante pour préparer la boisson séléctionnée.")
      println("Veuillez choisir une autre machine ou verifier les stock en mode Admin.")
      println()
      return false
    }

    if (!machine.removeIngredient("lait", laitrequis)) {
      println()
      println("Erreur : quantité de lait insuffisante pour préparer la boisson séléctionnée.")
      if ((choixboisson == 2) || (choixboisson == 3.1)) {
        println("Veuillez choisir une autre machine ou verifier les stocks en mode Admin.")
      }
      if ((choixboisson == 3.2) || (choixboisson == 3.3)) {
        println("Veuillez choisir une taille plus petite, choisir une autre machine ou vérifier les stocks en mode Admin.")
      }
      println()
      return false
    }

    // Une fois les stock (removeIngredient) validés et déduits, affichage du recapitulatif avec prix
    prixtotal = prixboisson + prixsucre + prixlait

    if (choixboisson == 1) {
      if (choixsucre == 1) {
        println()
        printf("Prix total : CHF %.2f", prixtotal) // Affichage du prix à deux décimales pour assurer un prix aux centimes (cohérent)
      }
      else {
        println()
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prixtotal)
      }
    }
    else {
      if (choixsucre == 1 && choixlait == 2)
        printf("Prix total: CHF %.2f", prixtotal)
      if ((choixsucre >= 2 && choixsucre <= 4) && (choixlait == 2))
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prixtotal)
      if (choixsucre == 1 && choixlait == 1)
        printf("Prix total :  CHF %.2f + CHF %.2f  =  CHF %.2f", prixboisson, prixlait, prixtotal)
      if ((choixsucre >= 2 && choixsucre <= 4) && (choixlait == 1))
        printf("Prix total :  CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prixboisson, prixsucre, prixlait, prixtotal)
    }

    // Paiement avec un code Twint aléatoire
    codeTwint = ""
    for (_ <- 1 to longeurcodeTwint) {
      var alphanumericaleatoire = alphanumeric(Random.nextInt(alphanumeric.length))
      codeTwint += alphanumericaleatoire
    }
    println()
    println()
    println("Veuillez payer en utilisant Twint.")
    println("Votre code de paiement est : " + codeTwint)
    println("(En attente de validation du paiement...)")
    Thread.sleep(3000) // attente de 3 secondes (3000 millisecondes)
    println("          ")
    println("Merci ! Votre paiement a été accepté.")
    println("Préparation de votre boisson...")
    println("[...]")
    println("Votre " + boissonselectionnee + " est prêt ! Bonne dégustation !")
    println()
    true
  }

  // 2.4) méthode pour modifier les stocks en mode Admin : appel des méthode add et remove ingrédients selon le choix
  def restockMachine(machine: Machine): Unit = {
    var choixactionstock = 0

    // Affichage des stocks actuels
    println()
    println("Niveaux de stock actuels : ")
    println("Poudre de café : " + machine.coffee + "g")
    printf("Lait :  %.3fL", machine.milk / 1000.0) // affichage du lait à 3 décimales pour la cohérence des mesures et /1000 pour l'affichage en L
    println()
    println("Sucre : " + machine.sugar + "g")
    println()

    // choix de l'action
    println("Souhaitez-vous ajouter ou retirer des Ingrédients ?")
    println("1) Ajouter")
    println("2) Retirer")
    println(">")
    choixactionstock = readInt()

    while (choixactionstock != 1 && choixactionstock != 2) {
      println("Veuillez séléctionner une entrée valide")
      println(">")
      choixactionstock = readInt()
    }

    // Réaprovisionnement des stocks: verification que les entrées sont valides et changement des variables pour enregistrer les ajouts
    if (choixactionstock == 1) {
      println("Veuillez entrer les quantités à ajouter : ")
      println("Poudre de café : ")
      println(">")
      var ajoutcafe = readInt()
      while (ajoutcafe < 0) {
        println("Veuillez entrer une valeur positive")
        println(">")
        ajoutcafe = readInt()
      }
      machine.addIngredient("cafe", ajoutcafe)

      println("Lait : ")
      println(">")
      var ajoutlait = (readDouble() * 1000).toInt // * 1000 pour stocker le lait en ml
      while (ajoutlait < 0) {
        println("Veuillez entrer une valeur positive")
        println(">")
        ajoutlait = (readDouble() * 1000).toInt
      }
      machine.addIngredient("lait", ajoutlait)

      println("Sucre : ")
      println(">")
      var ajoutsucre = readInt()
      while (ajoutsucre < 0) {
        println("Veuillez entrer une valeur positive")
        println(">")
        ajoutsucre = readInt()
      }
      machine.addIngredient("sucre", ajoutsucre)
    }
    // reduction des stocks
    if (choixactionstock == 2) {
      println("Veuillez entrer les quantités à retirer : ")
      println("Poudre de café : ")
      println(">")
      var reductioncafe = readInt()
      while (reductioncafe < 0) {
        println("Veuillez entrer une valeur positive")
        println(">")
        reductioncafe = readInt()
      }
      while (!machine.removeIngredient("cafe", reductioncafe)) {
        println("Stock de café insuffisant pour retirer la quantité requise. Veuillez entrer une valeur plus petite.")
        println(">")
        reductioncafe = readInt()
      }

      println("Lait : ")
      println(">")
      var reductionlait = (readDouble() * 1000).toInt // * 1000 pour stocker le lait en ml
      while (reductionlait < 0) {
        println("Veuillez entrer une valeur positive")
        println(">")
        reductionlait = (readDouble() * 1000).toInt
      }
      while (!machine.removeIngredient("lait", reductionlait)) {
        println("Stock de lait insuffisant pour retirer la quantité requise. Veuillez entrer une valeur plus petite.")
        println(">")
        reductionlait = (readDouble() * 100).toInt
      }

      println("Sucre : ")
      println(">")
      var reductionsucre = readInt()
      while (reductionsucre < 0) {
        println("Veuillez entrer une valeur positive")
        println(">")
        reductionsucre = readInt()
      }
      while (!machine.removeIngredient("sucre", reductionsucre)) {
        println("Stock de sucre insuffisant pour retirer la quantité requise. Veuillez entrer une valeur plus petite.")
        println(">")
        reductionsucre = readInt()
      }
    }
    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
    println()
  }

  // 2.5) Méthode pour la lecture du fichier CSV et gestion des exeptions
  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machine_chargee: ArrayBuffer[Machine] = ArrayBuffer[Machine]()
    try {
      val fr = Source.fromFile(filename)
      val lines = fr.getLines().drop(1)
      var machineId: Int = 1
      for (line <- lines) {
        if (!line.isEmpty) {
          val element = line.split(",")
          val pin = element(0)
          val lait = element(1).toInt
          val sucre = element(2).toInt
          val cafe = element(3).toInt
          machine_chargee += new Machine(machineId, pin, lait, sucre, cafe)
          machineId += 1
        }
      }
      machine_chargee
    }
    catch {
      case ex: java.io.FileNotFoundException =>
        println("Erreur: Fichier introuvable. Vérifier le chemin d'accès et réessayer.")
        null
    }
  }

  // 2.6) méthode pour l'enregistrement du fichier csv et gestion des exceptions
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val printWriter = new PrintWriter(new FileWriter(filename, false))
      printWriter.println("PINCODE,MILK,SUGAR,COFFEE")
      for (mach <- machines) {
        printWriter.println(mach.pincode + "," + mach.milk + "," + mach.sugar + "," + mach.coffee)
      }
      printWriter.close()
    } catch {
      case ex: java.io.FileNotFoundException =>
        println("Erreur: Echec de l'écriture dans machines.csv.")
        println("Le fichier peut être verrouiller ou en lecture seule.")
    }
  }

  // 2.7) méthode générale pour le mode Admin et appel des méthodes selon les actions
  def ModeAdmin(machine: Machine): Boolean = {
    var choixaction = 0
    if (!validatePin(machine)) {
      return false
    }
    println()
    println("Que souhaitez-vous faire ? ")
    println("1) Modifier les stocks ")
    println("2) Mettre à jour le code PIN ")
    println(">")
    choixaction = readInt()

    // vérification que l'entrée est valide (pour le choix de l'action)
    while (choixaction != 1 && choixaction != 2) {
      println("Veuillez sélectionner une entrée valide")
      println(">")
      choixaction = readInt()
    }
    if (choixaction == 2) {
      updatePin(machine)
    }
    if (choixaction == 1) {
      restockMachine(machine)
    }
    true
  }

  // 2.8 ) méthode générale pour le mode client et appel des méthode selon les actions
  def ModeClient(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    var machine = machines(machineId)
    var stockvalid: Boolean = false
    do {
      stockvalid = serveClient(machine)
      if (stockvalid) {
        return
      }
      println("Veuillez sélectionner une machine ( 1 - " + machines.length + " )")
      println(">")
      var id = readInt() - 1

      while (id < 0 || (id > machines.length - 1)) {
        println("Veuillez sélectionner une entrée valide")
        println(">")
        id = readInt() - 1
      }
      machine = machines(id)
    } while (!stockvalid)
  }

  // Etape 3: "corps" du programme en appelant les méthodes
  def main(args: Array[String]): Unit = {

    // variables pour le choix du mode et de la machine
    var choixmode = 0
    var machineId = 0

    // 3.1) chargement des machines depuis le fichier et gestion des exeptions
    println()
    println("Chargement des machines depuis machines.csv...")
    val machines = loadcsv("machines.csv")

    // gestion des erreurs
    if (machines == null) {
      println("Erreur : Echec du chargement ou de la sauvegarde des machines.")
      println("Fermeture du programme.")
      return
    }
    // Affichages des machines chargées
    for (i <- machines) {
      println()
      println("Machine " + i.Id + " chargée : ")
      println("   ID: " + i.Id)
      println("   Code PIN: " + i.pincode)
      printf("   Lait: %.3fL", i.milk / 1000.00)
      println()
      println("   Sucre: " + i.sugar + "g")
      println("   Café: " + i.coffee + "g")
    }
    println("")
    println(machines.length + " machine(s) chargée(s) avec succès.")

    // 3.2) sélection du mode et de la machine
    while (choixmode != 3) {
      println()
      println("       Nospresso Café    ")
      println("Veuillez sélectionner votre mode")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      println(">")
      choixmode = readInt()

      // vérification que l'entrée est valide (pour le choix du mode)
      while (choixmode != 1 && choixmode != 2 && choixmode != 3) {
        println("Veuillez sélectionner une entrée valide")
        println(">")
        choixmode = readInt()
      }

      if (choixmode == 1 || choixmode == 2) {
        println("Veuillez séléctionner une machine (1 - " + machines.length + ")")
        println(">")
        machineId = readInt() - 1

        // verification que l'entrée est valide (pour le choix de la machine)
        while (machineId < 0 || machineId > machines.length - 1) {
          println("Veuillez sélectionner une entrée valide")
          println(">")
          machineId = readInt() - 1
        }

        // 3.3) mode Client (appel de la methode generale)
        if (choixmode == 1) {
          ModeClient(machineId, machines)
        }
        // 3.4) mode Admin (appel de la methode generale)
        if (choixmode == 2) {
          if (!ModeAdmin(machines(machineId))) {
            choixmode = 3
          }
        }
      }
    }
    // 3.5) suvegarde des machines dans le fichier CSV et gestion des exceptions
    println()
    println("Sauvegarde de " + machines.length + " machines dans machines.csv...")
    try {
      savecsv("machines.csv", machines)
    } catch {
      case ex: java.io.IOException =>
        println("Erreur : Echec du chargement ou de la sauvegarde des machines.")
        println("Fermeture du programme.")
        return
    }
    println("Fichier sauvegardé avec succès.")
  }
}