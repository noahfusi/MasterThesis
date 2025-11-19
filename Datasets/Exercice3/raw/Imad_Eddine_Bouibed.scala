import scala.io.StdIn._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}
import java.io.PrintWriter


object Main {

  class Machine(val id: Int, var pincode: String, var milk: Int,
                var sugar: Int, var coffee: Int) {

    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (amount < 0) {
        println(s"Erreur : l'ajout de l'ingrédient '${ingredient}' ne peut pas être négatif. ($amount)")
      } else {
        ingredient match {
          case "milk" => milk += amount
          case "sugar" => sugar += amount
          case "coffee" => coffee += amount
          case _ => println(s"Erreur : L'ingrédient '$ingredient' est inconnu.")
        }
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      var isRemoved = false
      if (amount < 0) {
        println(s"Erreur : le retrait de l'ingrédient '$ingredient' ne peut pas être négatif. ($amount)")
      } else {
        ingredient match {
          case "milk" =>
            if (milk >= amount) { milk -= amount; isRemoved = true }
            else {println(s"Erreur : Stock insuffisant de lait. Demandé: $amount, Disponible: $milk")}
          case "sugar" =>
            if (sugar >= amount) { sugar -= amount; isRemoved = true }
            else {println(s"Erreur : Stock insuffisant de sucre. Demandé: $amount, Disponible: $sugar")}
          case "coffee" =>
            if (coffee >= amount) {coffee -= amount; isRemoved = true}
            else {println(s"Erreur : Stock insuffisant de café. Demandé: $amount, Disponible: $coffee")}
          case _ => println(s"Erreur : L'ingrédient '$ingredient' est inconnu.")
        }
      }
      isRemoved
    }

    def affiche(): Unit = {
      println(f"    ID: $id\n    Code PIN: $pincode\n    Lait: ${milk/1000.toDouble}%.3fL\n    Sucre: ${sugar}g\n    Café: ${coffee}g")
    }

    def write(): String = {
      val machinString = s"$pincode,$milk,$sugar,$coffee"
      machinString
    }
  }

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    println(s"Chargement des machines depuis $filename...\n...")
    Thread.sleep(1000)
    var fr: BufferedSource = null
    try {
      val machines: ArrayBuffer[Machine] = ArrayBuffer()
      fr = Source.fromFile(s"$filename")
      val lignefr = fr.reset.getLines.drop(1)
      var id = 0

      while (!lignefr.isEmpty) {
        id += 1
        val ligne = lignefr.next
        val value = ligne.split(",")
        val uneMachine = new Machine(id, value(0), value(1).toInt, value(2).toInt, value(3).toInt)
        machines.addOne(uneMachine)
        println(s"Machine $id chargée :")
        uneMachine.affiche()
      }
      if (machines.nonEmpty) println(s"\n$id machine(s) chargée(s) avec succès.")
      fr.close()
      machines
    } catch {
      case ex: java.io.FileNotFoundException =>
        println(s"Erreur : Le fichier ${filename} est introuvable. ${ex.getMessage}")
        ArrayBuffer[Machine]()
      case ex: java.io.IOException =>
        println(s"Erreur : Une erreur d'entrée/sortie s'est produite lors de la lecture de ${filename}. ${ex.getMessage}")
        ArrayBuffer[Machine]()
    }
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    println(s"Sauvegarde de ${machines.length} machines dans $filename...")
    Thread.sleep(1000)

    var fw: PrintWriter = null // Déclaration de fw en dehors du try
    try {
      fw = new PrintWriter(s"$filename") // Initialisation de fw dans le try
      fw.println("PINCODE,MILK,SUGAR,COFFEE")
      for (machine <- machines) {
        fw.println(machine.write())
      }
    } catch {
      case ex: java.io.IOException =>
        println(s"Erreur : Une erreur s'est produite lors de l'écriture dans le fichier $filename. ${ex.getMessage}")
    } finally {
      if (fw != null) {
        fw.close()
      }
    }
    println("Fichier sauvegardé avec succès.")
  }

  def machineSelect(machines: ArrayBuffer[Machine]): Machine = {
    val max = machines.length
    var machineId = 0
    do machineId = readLine(s"Machine sélectionnée (1-$max) > ").toInt
    while (machineId == 0 || machineId > max)
    machines(machineId)
  }

  def validatePin(machine: Machine): Boolean = {
    var pinIsCorrect = false
    println("Entrez le code PIN :")

    for (attempt <- 1 to 3 if !pinIsCorrect) {
      val pinInput = readLine(" > ")
      if (pinInput == machine.pincode) {
        println("Code PIN correct. Accès accordé.")
        pinIsCorrect = true
      } else {
        println(s"Attention! Code PIN incorrect. ${3 - attempt} tentative(s) restantes.")
        if (attempt == 3) println("\nTrop de tentatives échouées. Fin du programme.")
      }
    }
    pinIsCorrect
  }

  def updatePin(machine: Machine): Unit = {
    println(s"Mise à jour du code PIN pour la Machine ${machine.id}.")
    var newPin = ""
    do newPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    while (newPin.length != 6)
    machine.pincode = newPin
    println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal...")
    Thread.sleep(1000)
  }

  def serveClient(machine: Machine): Boolean = {
    var choix_boisson = 0
    var boisson = "rien"
    do choix_boisson = readLine("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ").toInt
    while (choix_boisson == 0 || choix_boisson > 3)

    //        Initialiser le prix et les quantités nécessaires pour la boisson à préparer et vérifier suffisance des stocks
    var prix_base     = 0.00
    var cafe_boisson  = 0
    var sucre_boisson = 0
    var lait_boisson  = 0

    if      (choix_boisson == 1)  {boisson = "Expresso"; prix_base = 2.00;  cafe_boisson = 8}
    else if (choix_boisson == 2)  {boisson = "Cappucino"; prix_base = 2.50;  cafe_boisson= 6; lait_boisson = 100}
    else if (choix_boisson == 3)  {boisson = "Latte"  // Si boisson Latte choisir quelle taille
      var taille_latte = 0
      do taille_latte = readLine("Veuillez sélectionner la taille de votre latte :\n1) Latte Petit - CHF 2.70\n2) Latte Moyen - CHF 3.20\n3) Latte Grand - CHF 3.70\n> ").toInt
      while (taille_latte == 0 || taille_latte > 3)
      if      (taille_latte == 1)  {boisson += " (Petit)"; prix_base = 2.70; cafe_boisson = 6; lait_boisson = 120}
      else if (taille_latte == 2)  {boisson += " (Moyen)"; prix_base = 3.20; cafe_boisson = 8; lait_boisson = 150}
      else if (taille_latte == 3)  {boisson += " (Grand)"; prix_base = 3.70; cafe_boisson = 12; lait_boisson = 200}
    }

    //  Supplément sucre ?
    var supp_sucre = 0
    var prix_sucre = 0.0
    var niveau_sucre = "Sans sucre"
    do supp_sucre = readLine("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n> ").toInt
    while (supp_sucre == 0 || supp_sucre > 4)
    if      (supp_sucre == 2)  {niveau_sucre = "Peu (5g)"; sucre_boisson = 5; prix_sucre = 0.10}
    else if (supp_sucre == 3)  {niveau_sucre = "Moyen (10g)"; sucre_boisson = 10; prix_sucre = 0.20}
    else if (supp_sucre == 4)  {niveau_sucre = "Beaucoup (15g)"; sucre_boisson = 15; prix_sucre = 0.30}

    //  Supplément lait ?
    var str_lait = "Non"
    var prix_lait = 0.0
    if (choix_boisson == 2 || choix_boisson == 3) { //  (uniquement pour Cappuccino ou Latte)
      var supp_lait = 0
      do supp_lait = readLine("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non\n> ").toInt
      while (supp_lait != 1 && supp_lait != 2)
      if (supp_lait == 1)  {
        supp_lait = 0
        do supp_lait = readLine("Combien de doses ? (maximum 3)\n> ").toInt
        while (supp_lait == 0 || supp_lait > 3)
        str_lait = s"Oui - $supp_lait dose(s)"
        lait_boisson += supp_lait * 50
        prix_lait = supp_lait * 0.05
      }
    }

    //        Résumé de la commande
    println(s"\nBoisson sélectionnée  : $boisson\nNiveau de sucre       : $niveau_sucre\nLait supplémentaire   : $str_lait")

    //        Si Stock insuffisant pour la boisson  -> erreur
    var error = false
    var stock_insuf = ""
    if  (lait_boisson > machine.milk)    {error = true; stock_insuf += " lait -"}
    if  (sucre_boisson > machine.sugar)  {error = true; stock_insuf += " sucre -"}
    if  (cafe_boisson > machine.coffee)  {error = true; stock_insuf += " poudre de café -"}

    if (error) {
      println(s"\nErreur : Quantité insuffisante de$stock_insuf \npour préparer la boisson sélectionnée.\nVeuillez choisir une autre machine.")
      Thread.sleep(1000)
    }     //        Sinon si stock suffisant procéder au payement
    else {
      val code_paiement = Random.shuffle("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").take(5)  // générer code de paiement aléatoire
      println(f"Prix total : CHF $prix_base%.2f + CHF ${prix_sucre + prix_lait}%.2f = CHF ${prix_base + prix_sucre + prix_lait}%.2f\n\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : $code_paiement\n(En attente de validation du paiement...)\n[...]")
      Thread.sleep(1000)
      println("\nMerci ! Votre paiement a été accepté.\nPréparation de votre boisson...\n[...]")
      Thread.sleep(1000)
      machine.removeIngredient("milk", lait_boisson)
      machine.removeIngredient("sugar", sucre_boisson)
      machine.removeIngredient("coffee", cafe_boisson)
//      machine.milk   -= lait_boisson
//      machine.sugar  -= sucre_boisson
//      machine.coffee -= cafe_boisson
      println(s"Votre $boisson est prêt ! Bonne dégustation !")
      Thread.sleep(1000)
    }
    return !error
  }

  def restockMachine(machine: Machine): Unit = {
    println(s"Accès autorisé.\nNiveaux des stocks actuels :\n  Poudre de café: ${machine.coffee}g" + s"\n  Sucre         : ${machine.sugar}g" + f"\n  Lait          : ${machine.milk.toDouble/1000}%.3f" + "L")
    println("Réapprovisionnement des stocks...\nEntrez les quantités à ajouter  :\n(Valeurs positives uniquement)")

    var refillMilk = 0.0
    var inputNotValidMilk = true
    do {
      try {
        refillMilk = readLine("  Lait           (L) > ").toDouble
        inputNotValidMilk = false
      } catch {
        case ex: NumberFormatException =>
          println(s"Erreur : Veuillez entrer une valeur numérique valide pour le lait. ${ex.getMessage}")
      }
    } while (inputNotValidMilk)

    var refillSugar = 0
    var inputNotValidSugar = true
    do {
      try {
        refillSugar = readLine("  Sucre          (g) > ").toInt
        inputNotValidSugar = false
      } catch {
        case ex: NumberFormatException =>
          println(s"Erreur : Veuillez entrer une valeur numérique valide pour le sucre. ${ex.getMessage}")
      }
    } while (inputNotValidSugar)

    var refillCoffee = 0
    var inputNotValidCoffee = true
    do {
      try {
        refillCoffee = readLine("  Poudre de café (g) > ").toInt
        inputNotValidCoffee = false
      } catch {
        case ex: NumberFormatException =>
          println(s"Erreur : Veuillez entrer une valeur numérique valide pour la poudre de café. ${ex.getMessage}")
      }
    } while(inputNotValidCoffee)

    machine.addIngredient("milk", (refillMilk * 1000).toInt)
    machine.addIngredient("sugar", refillSugar)
    machine.addIngredient("coffee", refillCoffee)
    println("Les stocks ont été mis à jour avec succès..\nRetour au menu principal...")
    Thread.sleep(1000)
  }

  // -------------------------------------------------------------------- //

  def main(args: Array[String]): Unit = {
    val machines = loadcsv("machines.csv")
    if (machines.isEmpty) {
      println("Erreur : Aucune machine n'a été chargée.\n         Le programme ne peut pas continuer.\n         Veuillez vérifier le fichier de données")
      System.exit(0)
    }

    //    Choix du mode :
    var mode = 0
    while (mode != 3) {
      do mode = readLine("\n      Nospresso Café\nVeuillez sélectionner votre mode :\n  1) Client\n  2) Admin\n  3) Quitter\n  > ").toInt
      while (mode == 0 || mode > 3)

      if ( mode == 2 ) {  //  Si mode Admin Vérifier PIN puis...
        println("Mode Admin")
        val machine = machineSelect(machines)

        if (validatePin(machine)) {     //    ... Choix MAJ PIN ou Réapprovisionnement :
          var adminChoice = 0
          do adminChoice = readLine("\nQue souhaitez vous faire en tant que admin :\n  1) Réapprovisionner le stock\n  2) Mettre à jour le code PIN\n  3) Quitter le mode admin\n  > ").toInt
          while (adminChoice == 0 || adminChoice > 3)

          if      (adminChoice == 1) restockMachine(machine)
          else if (adminChoice == 2) updatePin(machine)
          else if (adminChoice == 3) mode = 0
        } else { mode = 3 }
      }
      else if (mode == 1) { //  Si mode Client -> Servir
        val machine = machineSelect(machines)
        serveClient(machine)
      }

      //        Si mode Quitter -> quitter et sauvegarder dans le fichier
      else if (mode == 3) {
        println("Nospresso vous remercie pour votre visite.\nAu revoir !")
        Thread.sleep(500)
        savecsv("machines.csv", machines) // TODO décommenter
      }
    }
  }
}