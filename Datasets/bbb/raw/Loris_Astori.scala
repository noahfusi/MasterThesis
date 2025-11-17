import scala.io.StdIn.readLine
import scala.util.Random

object Main {

  val nbmachines = 5
  val machinepins = Array("434343", "434343", "434343", "434343", "434343")
  val cafestock = Array(50, 50, 50, 50, 50)
  val sucrestock = Array(30, 30, 30, 30, 30)
  val laitstock = Array(500, 500, 500, 500, 500)

  def main(args: Array[String]): Unit = {
    var continuer = true
    while (continuer) {
      println("Veuillez choisir votre mode : \n1) Client \n2) Admin \n3) Quitter")
      print("> ")
      val choixmode = readLine()

      if (choixmode == "1") {
        println("Veuillez sélectionner une machine (1-5):")
        print("> ")
        val machineId = readLine().toInt - 1

        if (machineId >= 0 && machineId < nbmachines) {
          println("Sélectionnée : Machine " + (machineId + 1))
          if (!serveClient(machineId, cafestock, sucrestock, laitstock)) {
            println("Impossible de servir la boisson, réessayez avec une autre machine ou vérifiez les stocks.")}
        } else {
          println("Choix de machine invalide. Veuillez choisir un numéro entre 1 et 5.")}

      } else if (choixmode == "2") {
        println("Veuillez sélectionner une machine (1-5):")
        print("> ")
        val machineId = readLine().toInt - 1

        if  (machineId >= 0 && machineId < nbmachines) {
          println("Sélectionnée : Machine " + (machineId + 1))
          if (!validatePin(machineId, machinepins)) {
            continuer = false
          } else {
            println("Accès autorisé. Que souhaitez-vous faire ? \n1) Réapprovisionner \n2) Modifier le code PIN")
            print("> ")
            val choixadmin = readLine()

            if (choixadmin == "1") {
              restockMachine(machineId, cafestock, sucrestock, laitstock)
            } else if (choixadmin == "2") {
              updatePin(machineId, machinepins)
            } else {
              println("Choix invalide.")}}
        } else {
          println("Choix de machine invalide. Veuillez choisir une machine entre 1 et 5.")}

      } else if (choixmode == "3") {
        continuer = false

      } else {
        println("Choix invalide, réessayez.")}}
    println("Fin du programme.")
  }


  def serveClient(machineId: Int, cafestock: Array[Int], sucrestock: Array[Int], laitstock: Array[Int]): Boolean = {
    println("Sélectionner votre boisson : \n 1) Expresso - CHF 2.00 \n 2) Cappuccino - CHF 2.50 \n 3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")

    val choixboisson = readLine()

    var cafe = 0
    var lait = 0
    var prixbase = 0.0

    if (choixboisson == "1") {
      cafe = 8
      prixbase = 2.00
    } else if (choixboisson == "2") {
      cafe = 6
      lait = 100
      prixbase = 2.50
    } else if (choixboisson == "3") {
      println("Choisissez la taille de votre Latte : \n1) Petit \n2) Moyen \n3) Grand")
      print("> ")
      val size = readLine()
      if (size == "1") {
        cafe = 6
        lait = 120
        prixbase = 2.70
      } else if (size == "2") {
        cafe = 8
        lait = 150
        prixbase = 3.20
      } else if (size == "3") {
        cafe = 12
        lait = 200
        prixbase = 3.70
      } else {
        println("Choix invalide.")
        return false}
    } else {
      println("Choix invalide.")
      return false}

    println("Souhaitez-vous ajouter du sucre ? \n 1) Sans sucre \n 2) Peu (5g) - CHF 0.10 \n 3) Moyen (10g) - CHF 0.20 \n 4) Beaucoup (15g) - CHF 0.30")
    print("> ")

    val choixsucre = readLine()

    // sucre variable
    var sucrequantite = 0
    var prixsucre = 0.0
    if (choixsucre == "1") {
      sucrequantite = 0
    } else if (choixsucre == "2") {
      sucrequantite = 5
      prixsucre = 0.1
    } else if (choixsucre == "3") {
      sucrequantite = 10
      prixsucre = 0.2
    } else if (choixsucre == "4") {
      sucrequantite = 15
      prixsucre = 0.3
    } else {
      println("Choix de sucre invalide.")
      return false}


    {var prixlaitsupplementaire = 0.0
      var laitquantite = 0

      if (choixboisson == "2" || choixboisson == "3") {
        println("Voulez-vous ajouter du lait supplémentaire ? (0.05 CHF par dose) \n 1) Oui \n 2) Non")
        print("> ")

        val choixlait = readLine()

        if (choixlait == "1") {
          println("Combien de dose (3 maximum) (Dose = 50ml) ?")
          print("> ")
          val laitsupplementaire = readLine()

          if (laitsupplementaire == "1") {
            laitquantite = 50
            prixlaitsupplementaire = 0.05}
          else if (laitsupplementaire == "2") {
            laitquantite = 100
            prixlaitsupplementaire = 0.10}
          else if (laitsupplementaire == "3") {
            laitquantite = 150
            prixlaitsupplementaire = 0.15}
          else {
            println("Choix de lait supplémentaire invalide.")
            return false}}}

      // En fonction des stocks
      if (laitstock(machineId) >= (lait + laitquantite) && sucrestock(machineId) >= sucrequantite && cafestock(machineId) >= cafe) {
        cafestock(machineId) -= cafe
        sucrestock(machineId) -= sucrequantite
        laitstock(machineId) -= (lait + laitquantite)

        val prixfinal = prixbase + prixsucre + prixlaitsupplementaire
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f\n", prixbase, prixsucre, prixlaitsupplementaire, prixfinal)

        // TWINT
        val alphanumeric = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        var code = ""
        for (_ <- 1 to 5) {
          code = code + alphanumeric(Random.nextInt(alphanumeric.length))}

        printf("Code Twint : %s\n", code)
        println("(Attente de validation du paiement)")
        Thread.sleep(3000)
        println("Merci! Votre paiement a été accepté.")
        println("Préparation de votre boisson... \n[...]")
        Thread.sleep(3000)
        printf("Votre boisson est prête ! Bonne dégustation !")
        Thread.sleep(1000)
        return true
      } else {
        if (cafestock(machineId) < cafe) println("Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        if (sucrestock(machineId) < sucrequantite) println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        if (laitstock(machineId) < (lait + laitquantite)) println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        return false}}}



  def validatePin(machineId: Int, machinepins: Array[String]): Boolean = {
    var attempts = 3

    while (attempts > 0) {
      printf("Entrez le code PIN pour la Machine %d :\n", machineId + 1)
      print("> ")
      val pin = readLine()

      if (pin == machinepins(machineId)) {
        println("Code PIN correct. Accès autorisé.")
        return true
      } else {
        attempts -= 1
        if (attempts > 0) {
          printf("Code PIN incorrect. Il vous reste %d tentative(s).\n", attempts)}}}
    println("Trop de tentatives échouées. Le programme va s'arrêter.")
    false}




  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    printf("Mise à jour du code PIN pour la Machine %d.\n", machineId + 1)
    var nouveaupinmachine = false

    while (!nouveaupinmachine) {
      printf("Entrez un nouveau code PIN à 6 chiffres :\n")
      print("> ")

      val nouveaupin = readLine()

      if (nouveaupin.length == 6) {
        var valide = true
        for (i <- 0 until 6) {
          if (nouveaupin(i) < '0' || nouveaupin(i) > '9') {
            valide = false}}

        if (valide) {
          machinePins(machineId) = nouveaupin
          println("Le code PIN a été mis à jour avec succès. \nRetour au menu principal...")
          nouveaupinmachine = true
        } else {
          println("Le PIN doit avoir exactement 6 chiffres.")}}}}


  def restockMachine(machineId: Int, cafestock: Array[Int], sucrestock: Array[Int], laitstock: Array[Int]): Unit = {
    printf("Réapprovisionnement de la Machine %d.\n", machineId + 1)
    printf("Stock actuel de café en grammes: %dg\n", cafestock(machineId))
    printf("Stock actuel de sucre en grammes: %dg\n", sucrestock(machineId))
    printf("Stock actuel de lait en litres: %.2fL\n", laitstock(machineId)/ 1000.0)

    println("Combien de café ajouter ?")
    print("> ")
    val cafeajoute = readLine().toInt

    println("Combien de sucre ajouter ?")
    print("> ")
    val sucreajoute = readLine().toInt

    println("Combien de lait ajouter (en litres) ?")
    print("> ")
    val laitajoute = ((readLine().toDouble) * 1000) .toInt

    cafestock(machineId) = cafestock(machineId) + cafeajoute
    sucrestock(machineId) = sucrestock(machineId) + sucreajoute
    laitstock(machineId) = laitstock(machineId) + laitajoute

    printf("Stock de café : %d g \n Stock de sucre : %d g \n Stock de lait : %d L \n ", cafestock(machineId), sucrestock(machineId), (laitstock(machineId) / 1000))}
}

