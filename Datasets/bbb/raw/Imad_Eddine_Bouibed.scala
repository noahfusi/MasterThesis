import io.StdIn._
import scala.util.Random

object Main {
  val nbMachines = 5
  val coffeeStocks = Array.fill(nbMachines)(50)  // Initialisé à 50g
  val sugarStocks = Array.fill(nbMachines)(30)  // Initialisé à 30g
  val milkStocks = Array.fill(nbMachines)(0.5)  // Initialisé à 0.5L
  val machinePins = Array.fill(nbMachines)("434343")

  def machineSelect(): Int = {
    var machineId = 0
    do machineId = readLine("Machine sélectionnée (1-5) > ").toInt
    while (machineId == 0 || machineId > 5)
    return machineId
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var pinIsCorrect = false
    println("Entrez le code PIN :")

    for (attempt <- 1 to 3 if !pinIsCorrect) {
      val pinInput = readLine(" > ")
      if (pinInput == machinePins(machineId-1)) {
        println("Code PIN correct. Accès accordé.")
        pinIsCorrect = true
      } else {
        println(s"Attention! Code PIN incorrect. ${3 - attempt} tentative(s) restantes.")
        if (attempt == 3) println("\nTrop de tentatives échouées. Fin du programme.")
      }
    }
    pinIsCorrect
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println(s"Mise à jour du code PIN pour la Machine ${machineId+1}.")
    var newPin = ""
    do newPin = readLine("Entrez un nouveau code PIN à 6 chiffres > ")
    while (newPin.length != 6)
    machinePins(machineId) = newPin
    println("Le code PIN a été mis à jour avec succès.\nRetour au menu principal...")
    Thread.sleep(1000)
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Double]): Boolean = {
    var choix_boisson = 0
    var boisson = "rien"
    do choix_boisson = readLine("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n> ").toInt
    while (choix_boisson == 0 || choix_boisson > 3)

    //        Initialiser le prix et les quantités nécessaires pour la boisson à préparer et vérifier suffisance des stocks
    var prix_base     = 0.00
    var cafe_boisson  = 0
    var sucre_boisson = 0
    var lait_boisson  = 0.0

    if      (choix_boisson == 1)  {boisson = "Expresso"; prix_base = 2.00;  cafe_boisson = 8}
    else if (choix_boisson == 2)  {boisson = "Cappucino"; prix_base = 2.50;  cafe_boisson= 6; lait_boisson = 0.1}
    else if (choix_boisson == 3)  {boisson = "Latte"  // Si boisson Latte choisir quelle taille
      var taille_latte = 0
      do taille_latte = readLine("Veuillez sélectionner la taille de votre latte :\n1) Latte Petit - CHF 2.70\n2) Latte Moyen - CHF 3.20\n3) Latte Grand - CHF 3.70\n> ").toInt
      while (taille_latte == 0 || taille_latte > 3)
      if      (taille_latte == 1)  {boisson += " (Petit)"; prix_base = 2.70; cafe_boisson = 6; lait_boisson = 0.120}
      else if (taille_latte == 2)  {boisson += " (Moyen)"; prix_base = 3.20; cafe_boisson = 8; lait_boisson = 0.150}
      else if (taille_latte == 3)  {boisson += " (Grand)"; prix_base = 3.70; cafe_boisson = 12; lait_boisson = 0.200}
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
        lait_boisson += supp_lait * 0.050
        prix_lait = supp_lait * 0.05
      }
    }

    //        Résumé de la commande
    println(s"\nBoisson sélectionnée  : $boisson\nNiveau de sucre       : $niveau_sucre\nLait supplémentaire   : $str_lait")

    //        Si Stock insuffisant pour la boisson  -> erreur
    var error = false
    var stock_insuf = ""
    if  (cafe_boisson > coffeeStocks(machineId-1))  {error = true; stock_insuf += " -poudre de café-"}
    if  (lait_boisson > milkStocks(machineId-1))    {error = true; stock_insuf += " -lait-"}
    if  (sucre_boisson > sugarStocks(machineId-1))  {error = true; stock_insuf += " -sucre-"}

    if (error) {
      println(s"\nErreur : Quantité insuffisante de$stock_insuf \npour préparer la boisson sélectionnée.\nVeuillez choisir une autre machine.")
      Thread.sleep(1000)
    }
    //        Sinon si stock suffisant procéder au payement
    else {
      val code_paiement = Random.shuffle("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").take(5)  // générer code de paiement aléatoire
      println(f"Prix total : CHF $prix_base%.2f + CHF ${prix_sucre + prix_lait}%.2f = CHF ${prix_base + prix_sucre + prix_lait}%.2f\n\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : $code_paiement\n(En attente de validation du paiement...)\n[...]")
      Thread.sleep(1000)
      println("\nMerci ! Votre paiement a été accepté.\nPréparation de votre boisson...\n[...]")
      Thread.sleep(1000)
      coffeeStocks(machineId-1) -= cafe_boisson
      sugarStocks(machineId-1)  -= sucre_boisson
      milkStocks(machineId-1)   -= lait_boisson
      println(s"Votre $boisson est prêt ! Bonne dégustation !")
      Thread.sleep(1000)
    }
    return !error
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int],sugarStocks: Array[Int], milkStocks: Array[Double]): Unit = {
    println(s"Accès autorisé.\n\nStocks:\n  Poudre de café: ${coffeeStocks(machineId)}g" + s"\n  Sucre         : ${sugarStocks(machineId)}g" + f"\n  Lait          : ${milkStocks(machineId)}%.3f" + "L")
    println("Réapprovisionnement des stocks...\nEntrez les quantités à ajouter  :\n(Valeurs positives uniquement)")
    var refillCoffee = 0
    var refillMilk = 0.0
    var refillSugar = 0
    do refillCoffee = readLine("  Poudre de café > ").toInt while (refillCoffee < 0)
    do refillSugar = readLine("  Sucre          > ").toInt while (refillSugar < 0)
    do refillMilk = readLine("  Lait           > ").toDouble while (refillMilk < 0)
    coffeeStocks(machineId) += refillCoffee
    milkStocks(machineId)   += refillMilk
    sugarStocks(machineId)  += refillSugar
    println("Les stocks ont été mis à jour avec succès..\nRetour au menu principal...")
    Thread.sleep(1000)
  }

  // -------------------------------------------------------------------- //

  def main(args: Array[String]): Unit = {
    //    Choix du mode :
    var mode = 0
    while (mode != 3) {
      do mode = readLine("\nNospresso Café\nVeuillez sélectionner votre mode :\n  1) Client\n  2) Admin\n  3) Quitter\n  > ").toInt
      while (mode == 0 || mode > 3)

      if ( mode == 2 ) {  //  Si mode Admin Vérifier PIN puis...
        println("Mode Admin")
        val machineId = machineSelect()

        if (validatePin(machineId, machinePins)) {
          //    ... Choix MAJ PIN ou Réapprovisionnement :
          var adminChoice = 0
          do adminChoice = readLine("\nQue souhaitez vous faire en tant que admin :\n  1) Mettre à jour le PIN\n  2) Réapprovisionner le stock\n  > ").toInt
          while (adminChoice == 0 || adminChoice > 2)

          if      (adminChoice == 1) updatePin(machineId-1, machinePins)
          else if (adminChoice == 2) restockMachine(machineId-1, coffeeStocks, sugarStocks, milkStocks)
        } else { mode = 3 }
      }
      else if (mode == 1) { //  Si mode Client -> Servir
        val machineId = machineSelect()
        serveClient(machineId, coffeeStocks, sugarStocks, milkStocks)
      }

      //        Si mode Quitter -> quitter sans rien faire
      else if (mode == 3) println("Nospresso vous remercie pour votre visite.\nAu revoir !")
    }
  }

}
