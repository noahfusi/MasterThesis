import scala.io.StdIn._
import scala.util.Random
object Main {

  var nbmachine = 0
  var code = Array("434343","434343","434343","434343","434343")
  var mdp = ""
  var coffeeStocks = Array(50,50,50,50,50)
  var sugarStocks = Array(30,30,30,30,30)
  var milkStocks = Array(500,500,500,500,500)
  var ajoutcafe = 0
  var ajoutsucre = 0
  var ajoutlait = 0
  val prixE = 2.00
  val prixC = 2.50
  val prixLP = 2.70
  val prixLM = 3.20
  val prixLG = 3.70
  val sucreP = 0.10
  val sucreM = 0.20
  val sucreB = 0.30
  val prixLait = 0.05
  var prixfinal = 0.0
  var boisson = 0
  var sucre = 0
  var lait = 0
  var dose = 0
  var latte = 0

  def validatePin(machineId : Int, machinePins : Array[String]): Boolean={
    var machineId = nbmachine
    var machinePins = code
    if(machinePins(machineId - 1) == mdp ){
      return true
    }
    else return false
  }

  def updatePin(machineId :Int, machinePins :Array[String]): Unit={
    var machineId = nbmachine
    var machinePins = code
    println("Mise à jour du code PIN pour la machine " + machineId)
    do{
      code(machineId - 1) = readLine("Entrez un nouveau code Pin à 6 chiffres >")
    }while((code(machineId -1).length) != 6)
    println("Le code PIN à été mis à jour avec succès. \n" + "Retour au menu principal...")
  }

  def restockMachine(machineID : Int, coffeeStocks : Array[Int], sugarStocks :Array[Int], milkStocks : Array[Int]): Unit={
    var machineID = nbmachine

    do {
      ajoutcafe = readLine("   Poudre de café ( en grammes ) : ").toInt
    }while(ajoutcafe < 0)

    do {
      ajoutsucre =readLine("   Sucre ( en grammes )         : ").toInt
    }while( ajoutsucre < 0)

    do{
      ajoutlait =readLine("   Lait ( en millilitres )          : ").toInt
    }while( ajoutlait < 0)
    coffeeStocks(machineID - 1) += ajoutcafe
    milkStocks(machineID - 1) += ajoutlait
    sugarStocks(machineID -1) += ajoutsucre
    println("Niveaux de stock mis à jour. \n" + "Retour au menu principal...")
  }

  def serveClient(machineId : Int, coffeeStocks : Array[Int], sugarStocks : Array[Int], milkStocks : Array[Int]): Boolean={
    do {
      nbmachine = readLine("Machine sélectionnée (1-5) ? >").toInt
    }while(!(nbmachine == 1 || nbmachine == 2 || nbmachine == 3 || nbmachine == 4 || nbmachine == 5))
    var machineID = nbmachine
    var p1 = Random.alphanumeric(1).toString
    var p2 = Random.alphanumeric(1).toString
    var p3 = Random.alphanumeric(1).toString
    var p4 = Random.alphanumeric(1).toString
    var p5 = Random.alphanumeric(1).toString
    var paiement = p1 + p2 + p3 +p4 + p5

    do{
      boisson = readLine("        Veuillez sélectionner votre boisson  \n" + "1) Expresso - CHF 2.00  \n" + "2) Cappuccino - CHF 2.50 \n" + "3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)" + "\n").toInt
    } while (!(boisson == 1 || boisson == 2 || boisson == 3))
    if( boisson == 3){
      do{
        latte = readLine("1) Petit \n" + "2) Moyen \n" + "3) Grand" + "\n").toInt
      } while (!(latte == 1 || latte == 2 || latte == 3))
    }
    do{
      sucre = readLine("Souhaitez-vous ajouter du sucre ?  \n" + "1) Sans sucre \n" + "2) Peu (5g) - CHF 0.10  \n" + "3) Moyen (10g) - CHF 0.20  \n" + "4) Beaucoup (15g) - CHF 0.30" + "\n").toInt
    } while (!(sucre == 1 || sucre == 2 || sucre == 3 || sucre == 4))
    if ((boisson == 2) || (boisson == 3)){
      do{
        lait = readLine("Souhaitez-vous ajouter du lait en supplément ? \n" + "1) Oui \n" + "2) Non" + "\n").toInt
        if(lait == 1){
          do{
            dose = readLine("Combien de dose ? (Dose - CHF 0.05, maximum 3 doses)" + "\n").toInt
          } while(!(dose == 1 || dose == 2 || dose == 3))
        }
      } while(!(lait == 1 || lait == 2))
    }

    // Expresso ( valable )
    if((boisson == 1) && (sucre == 1) && (coffeeStocks(machineID -1 ) >= 8)){
      var prixfinal = prixE
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Sans sucre \n" + "Prix total : CHF %.2f \n" + " \n" , prixfinal )
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Expresso est prêt ! Bonne dégustation !")
      coffeeStocks(machineID -1 ) -= 8
      return true
    }
    else if((boisson == 1) && (sucre == 2) && (coffeeStocks(machineID - 1) >= 8) && (sugarStocks(machineID - 1) >= 5)){
      var prixfinal = prixE + sucreP
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Peu (5g) \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixE, sucreP, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Expresso est prêt ! Bonne dégustation !")
      coffeeStocks(machineID -1 ) -= 8
      sugarStocks(machineID - 1) -= 5
      return true
    }
    else if((boisson == 1) && (sucre == 3) && (coffeeStocks(machineID - 1) >= 8) && (sugarStocks(machineID - 1) >= 10)){
      var prixfinal = prixE + sucreM
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Moyen (10g) \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixE, sucreM, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Expresso est prêt ! Bonne dégustation !")
      coffeeStocks(machineID -1 ) -= 8
      sugarStocks(machineID - 1) -= 10
      return true
    }
    else if((boisson == 1) && (sucre == 4) && (coffeeStocks(machineID- 1) >= 8) && (sugarStocks(machineID - 1) >= 15)){
      var prixfinal = prixE + sucreB
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixE, sucreB, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Expresso est prêt ! Bonne dégustation !")
      coffeeStocks(machineID -1 ) -= 8
      sugarStocks(machineID - 1) -= 15
      return true
    }

    // Expresso ( avec erreur de cafe )
    else if((boisson == 1) && (sucre == 1) && (coffeeStocks(machineID - 1) < 8)){
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Sans sucre \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 1) && (sucre == 2) && (coffeeStocks(machineID - 1) < 8)){
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Peu (5g) \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 1) && (sucre == 3) && (coffeeStocks(machineID - 1) < 8)){
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Moyen (10g) \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 1) && (sucre == 4) && (coffeeStocks(machineID - 1) < 8)){
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Beaucoup (15g) \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }

    // Expresso ( avec erreur de sucre )
    else if((boisson == 1) && (sucre == 2) && (sugarStocks(machineID - 1) < 5)){
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Peu (5g) \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 1) && (sucre == 3) && (sugarStocks(machineID - 1) < 10)){
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Moyen (10g) \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 1) && (sucre == 4) && (sugarStocks(machineID - 1) < 15)){
      printf("Boisson sélectionnée : Expresso \n" + "Niveau de sucre : Beaucoup (15g) \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }

    // Cappuccino ( valable)
    if((boisson == 2) && (sucre == 1) && (lait == 2) && (coffeeStocks(machineID - 1) >= 6) && (milkStocks(machineID - 1) >= 100)){
      var prixfinal = prixC
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f \n" + " \n" , prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 6
      milkStocks(machineID - 1) -= 100
      return true
    }
    else if((boisson == 2) && (sucre == 2) && (lait == 2) && (coffeeStocks(machineID - 1) >= 6) && (sugarStocks(machineID - 1) >= 5) && (milkStocks(machineID - 1) >= 100)){
      var prixfinal = prixC + sucreP
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixC, sucreP, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 6
      sugarStocks(machineID - 1) -= 5
      milkStocks(machineID - 1) -= 100
      return true
    }
    else if((boisson == 2) && (sucre == 3) && (lait == 2) && (coffeeStocks(machineID - 1) >= 6) && (sugarStocks(machineID - 1) >= 10) && (milkStocks(machineID - 1) >= 100)){
      var prixfinal = prixC + sucreM
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + "\n" , prixC, sucreM, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 6
      sugarStocks(machineID - 1) -= 10
      milkStocks(machineID - 1) -= 100
      return true
    }
    else if((boisson == 2) && (sucre == 4) && (lait == 2) && (coffeeStocks(machineID - 1) >= 6) && (sugarStocks(machineID - 1) >= 15) && (milkStocks(machineID - 1) >= 100)){
      var prixfinal = prixC + sucreB
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixC, sucreB, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 6
      sugarStocks(machineID - 1) -= 15
      milkStocks(machineID - 1) -= 100
      return true
    }
    else if((boisson == 2) && (sucre == 1) && (lait == 1) && (coffeeStocks(machineID - 1) >= 6) && (milkStocks(machineID - 1) >= 100 + (50 * dose))){
      var prixfinal = prixC + (prixLait * dose)
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixC, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 6
      milkStocks(machineID - 1) -= 100 + (50 * dose)
      return true
    }
    else if ((boisson == 2) && (sucre == 2) && (lait == 1) && (coffeeStocks(machineID - 1) >= 6) && (sugarStocks(machineID - 1) >= 5) && (milkStocks(machineID - 1) >= 100 + (50 * dose))){
      var prixfinal = prixC + sucreP + (prixLait * dose)
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixC, sucreP, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 6
      sugarStocks(machineID - 1) -= 5
      milkStocks(machineID - 1) -= 100 + ( 50 * dose)
      return true
    }
    else if((boisson == 2) && (sucre == 3) && (lait == 1) && (coffeeStocks(machineID - 1) >= 6) && (sugarStocks(machineID - 1) >= 10) && (milkStocks(machineID - 1) >= 100 + (50 * dose))){
      var prixfinal = prixC + sucreM + (prixLait * dose)
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixC, sucreM, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 6
      sugarStocks(machineID - 1) -= 10
      milkStocks(machineID - 1) -= 100 + ( 50 * dose)
      return true
    }
    else if((boisson == 2) && (sucre == 4) && (lait == 1) && (coffeeStocks(machineID - 1) >= 6) && (sugarStocks(machineID - 1) >= 15) && (milkStocks(machineID - 1) >= 100 + (50 * dose))){
      var prixfinal = prixC + sucreB + (prixLait * dose)
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixC, sucreB, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Cappuccino est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 6
      sugarStocks(machineID - 1) -= 15
      milkStocks(machineID - 1) -= 100 + ( 50 * dose)
      return true
    }

    // Cappuccino ( avec erreur de cafe)
    if((boisson == 2) && (sucre == 1) && (lait == 2) && (coffeeStocks(machineID - 1) < 6)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 2) && (sucre == 2) && (lait == 2) && (coffeeStocks(machineID - 1) < 6)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 2) && (sucre == 3) && (lait == 2) && (coffeeStocks(machineID - 1) < 6)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 2) && (sucre == 4) && (lait == 2) && (coffeeStocks(machineID - 1) < 6) ){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 2) && (sucre == 1) && (lait == 1) && (coffeeStocks(machineID - 1) < 6)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if ((boisson == 2) && (sucre == 2) && (lait == 1) && (coffeeStocks(machineID - 1) < 6)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 2) && (sucre == 3) && (lait == 1) && (coffeeStocks(machineID - 1) < 6)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 2) && (sucre == 4) && (lait == 1) && (coffeeStocks(machineID - 1) < 6)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }

    // Cappuccino ( avec erreur de sucre)
    if((boisson == 2) && (sucre == 2) && (lait == 2) && (sugarStocks(machineID - 1) < 5)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 2) && (sucre == 3) && (lait == 2) && (sugarStocks(machineID - 1) < 10)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 2) && (sucre == 4) && (lait == 2) && (sugarStocks(machineID - 1) < 15)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if ((boisson == 2) && (sucre == 2) && (lait == 1) && (sugarStocks(machineID - 1) < 5)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 2) && (sucre == 3) && (lait == 1) && (sugarStocks(machineID - 1) < 10)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 2) && (sucre == 4) && (lait == 1) && (sugarStocks(machineID - 1) < 15)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }

    // Cappuccino ( avec erreur de lait)
    if((boisson == 2) && (sucre == 1) && (lait == 2) && (milkStocks(machineID - 1) < 100)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 2) && (sucre == 2) && (lait == 2) && (milkStocks(machineID - 1)< 100)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 2) && (sucre == 3) && (lait == 2) && (milkStocks(machineID - 1) < 100)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 2) && (sucre == 4) && (lait == 2) && (milkStocks(machineID - 1) < 100)){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 2) && (sucre == 1) && (lait == 1) && (milkStocks(machineID - 1) < 100 + (50 * dose))){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if ((boisson == 2) && (sucre == 2) && (lait == 1) && (milkStocks(machineID - 1) < 100 + (50 * dose))){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 2) && (sucre == 3) && (lait == 1) && (milkStocks(machineID - 1) < 100 + (50 * dose))){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 2) && (sucre == 4) && (lait == 1) && (milkStocks(machineID - 1) < 100 + (50 * dose))){
      printf("Boisson sélectionnée : Cappuccino \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }

    // Latte Petit ( valable)
    if((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 2) && (coffeeStocks(machineID -1 ) >= 6) && (milkStocks(machineID -1) >= 120)){
      var prixfinal = prixLP
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f \n" + " \n" , prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte est prêt ! Bonne dégustation !")
      coffeeStocks(machineID -1 ) -= 6
      milkStocks(machineID -1) -= 120
      return true
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (coffeeStocks(machineID -1 ) >= 6) && (sugarStocks(machineID -1) >= 5) && (milkStocks(machineID -1) >= 120)){
      var prixfinal = prixLP + sucreP
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLP, sucreP, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID -1 ) -= 6
      sugarStocks(machineID -1) -= 5
      milkStocks(machineID -1)-= 120
      return true
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (coffeeStocks(machineID -1 ) >= 6) && (sugarStocks(machineID -1) >= 10) && (milkStocks(machineID -1) >= 120)){
      var prixfinal = prixLP + sucreM
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLP, sucreM, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID -1 ) -= 6
      sugarStocks(machineID -1) -= 10
      milkStocks(machineID -1) -= 120
      return true
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (coffeeStocks(machineID -1 ) >= 6) && (sugarStocks(machineID -1) >= 15) && (milkStocks(machineID -1) >= 120)){
      var prixfinal = prixLP + sucreB
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLP, sucreB, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID -1 ) -= 6
      sugarStocks(machineID -1) -= 15
      milkStocks(machineID -1) -= 120
      return true
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 1) && (coffeeStocks(machineID -1 ) >= 6) && (milkStocks(machineID -1) >= 120 + ( 50 * dose))){
      var prixfinal = prixLP + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n", prixLP, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID -1 ) -= 6
      milkStocks(machineID -1) -= 120 + ( 50 * dose)
      return true
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 1) && (coffeeStocks(machineID -1 ) >= 6) && (sugarStocks(machineID -1) >= 5) && (milkStocks(machineID -1) >= 120 + ( 50 * dose))){
      var prixfinal = prixLP + sucreP + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLP, sucreP, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID -1 )-= 6
      sugarStocks(machineID -1) -= 5
      milkStocks(machineID -1) -= 120 + ( 50 * dose)
      return true
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 1) && (coffeeStocks(machineID -1 ) >= 6) && (sugarStocks(machineID -1) >= 10) && (milkStocks(machineID -1) >= 120 + ( 50 * dose))){
      var prixfinal = prixLP + sucreM + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLP, sucreM, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID -1 ) -= 6
      sugarStocks(machineID -1) -= 10
      milkStocks(machineID -1) -= 120 + ( 50 * dose)
      return true
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 1) && (coffeeStocks(machineID -1 ) >= 6) && (sugarStocks(machineID -1) >= 15) && (milkStocks(machineID -1) >= 120 + ( 50 * dose))){
      var prixfinal = prixLP + sucreB + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f \n" + " \n" , prixLP, sucreB, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (petit) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID -1 ) -= 6
      sugarStocks(machineID -1) -= 15
      milkStocks(machineID -1) -= 120 + ( 50 * dose)
      return true
    }

    // latte petit ( avec probleme de cafe)
    if((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 2) && (coffeeStocks(machineID -1 ) < 6)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (coffeeStocks(machineID -1 ) < 6)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (coffeeStocks(machineID -1 ) < 6)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (coffeeStocks(machineID -1 ) < 6)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 1) && (coffeeStocks(machineID -1 )< 6)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 1) && (coffeeStocks(machineID -1 ) < 6)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 1) && (coffeeStocks(machineID -1 ) < 6)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 1) && (coffeeStocks(machineID -1 ) < 6)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }

    // Latte petit ( avec erreur de sucre)
    if((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (sugarStocks(machineID -1) < 5)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (sugarStocks(machineID -1) < 10)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (sugarStocks(machineID -1) < 15)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 1) && (sugarStocks(machineID -1) < 5)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 1) && (sugarStocks(machineID -1) < 10)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 1) && (sugarStocks(machineID -1) < 15)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }

    // Latte Petit ( avec erreur de lait)
    if((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 2) && (milkStocks(machineID -1) < 120)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 2) && (milkStocks(machineID -1) < 120)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 2) && (milkStocks(machineID -1) < 120)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 2) && (milkStocks(machineID -1) < 120)){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 1) && (lait == 1) && (milkStocks(machineID -1) < 120 + ( 50 * dose))){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if ((boisson == 3) && (latte == 1) && (sucre == 2) && (lait == 1) && (milkStocks(machineID -1) < 120 + ( 50 * dose))){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 3) && (lait == 1) && (milkStocks(machineID -1) < 120 + ( 50 * dose))){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 1) && (sucre == 4) && (lait == 1) && (milkStocks(machineID -1) < 120 + ( 50 * dose))){
      printf("Boisson sélectionnée : Latte (Petit) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }

    // Latte Moyen ( valable )
    if((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 2) && (coffeeStocks(machineID - 1) >= 8) && (milkStocks(machineID - 1) >= 150)){
      var prixfinal = prixLM
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f \n" + " \n" , prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 8
      milkStocks(machineID - 1) -= 150
      return true
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 2) && (coffeeStocks(machineID - 1) >= 8) && (sugarStocks(machineID - 1) >= 5) && (milkStocks(machineID - 1) >= 150)){
      var prixfinal = prixLM + sucreP
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixLM, sucreP, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 8
      sugarStocks(machineID - 1) -= 5
      milkStocks(machineID - 1) -= 150
      return true
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 2) && (coffeeStocks(machineID - 1) >= 8) && (sugarStocks(machineID - 1) >= 10) && (milkStocks(machineID - 1) >= 150)){
      var prixfinal = prixLM + sucreM
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixLM, sucreM, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 8
      sugarStocks(machineID - 1) -= 10
      milkStocks(machineID - 1) -= 150
      return true
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 2) && (coffeeStocks(machineID - 1) >= 8) && (sugarStocks(machineID - 1) >= 15) && (milkStocks(machineID - 1) >= 150)){
      var prixfinal = prixLM + sucreB
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixLM, sucreB, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 8
      sugarStocks(machineID - 1) -= 15
      milkStocks(machineID - 1) -= 150
      return true
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 1) && (coffeeStocks(machineID - 1) >= 8) && (milkStocks(machineID - 1) >= 150 + (50 * dose))){
      var prixfinal = prixLM + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixLM, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 8
      milkStocks(machineID - 1) -= 150 + ( 50 * dose)
      return true
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 1) && (coffeeStocks(machineID - 1) >= 8) && (sugarStocks(machineID - 1) >= 5) && (milkStocks(machineID - 1) >= 150 + (50 * dose))){
      var prixfinal = prixLM + sucreP + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixLM, sucreP, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 8
      sugarStocks(machineID - 1) -= 5
      milkStocks(machineID - 1) -= 150 + ( 50 * dose)
      return true
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 1) && (coffeeStocks(machineID - 1) >= 8) && (sugarStocks(machineID - 1) >= 10) && (milkStocks(machineID - 1) >= 150 + (50 * dose))){
      var prixfinal = prixLM + sucreM + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixLM, sucreM, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 8
      sugarStocks(machineID - 1) -= 10
      milkStocks(machineID - 1) -= 150 + ( 50 * dose)
      return true
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 1) && (coffeeStocks(machineID - 1) >= 8) && (sugarStocks(machineID - 1) >= 15) && (milkStocks(machineID - 1) >= 150 + (50 * dose))){
      var prixfinal = prixLM + sucreB + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixLM, sucreB, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (moyen) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 8
      sugarStocks(machineID - 1) -= 15
      milkStocks(machineID - 1) -= 150 + ( 50 * dose)
      return true
    }

    // latte moyen ( avec probleme de cafe)
    if((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 2) && (coffeeStocks(machineID - 1) < 8)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 2) && (coffeeStocks(machineID - 1) < 8)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 2) && (coffeeStocks(machineID - 1) < 8)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 2) && (coffeeStocks(machineID - 1) < 8)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 1) && (coffeeStocks(machineID - 1) < 8)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 1) && (coffeeStocks(machineID - 1) < 8)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 1) && (coffeeStocks(machineID - 1) < 8)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 1) && (coffeeStocks(machineID - 1) < 8)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }

    // Latte moyen ( avec erreur de sucre)
    if((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 2) && (sugarStocks(machineID - 1) < 5)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 2) && (sugarStocks(machineID - 1) < 10)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 2) && (sugarStocks(machineID - 1) < 15)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 1) && (sugarStocks(machineID - 1) < 5)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 1) && (sugarStocks(machineID - 1) < 10)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 1) && (sugarStocks(machineID - 1) < 15)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }

    // Latte moyen ( avec erreur de lait)
    if((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 2) && (milkStocks(machineID - 1) < 150)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 2) && (milkStocks(machineID - 1) < 150)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 2) && (milkStocks(machineID - 1) < 150)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 2) && (milkStocks(machineID - 1) < 150)){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 1) && (lait == 1) && (milkStocks(machineID - 1) < 150 + (50 * dose))){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if ((boisson == 3) && (latte == 2) && (sucre == 2) && (lait == 1) && (milkStocks(machineID - 1) < 150 + (50 * dose))){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 3) && (lait == 1) && (milkStocks(machineID - 1) < 150 + (50 * dose))){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 2) && (sucre == 4) && (lait == 1) && (milkStocks(machineID - 1) < 150 + (50 * dose))){
      printf("Boisson sélectionnée : Latte (Moyen) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }

    // Latte Grand ( valable)
    if((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 2) && (coffeeStocks(machineID - 1) >= 12) && (milkStocks(machineID - 1) >= 200)){
      var prixfinal = prixLG
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f \n" + " \n" , prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 12
      milkStocks(machineID - 1) -= 200
      return true
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 2) && (coffeeStocks(machineID - 1) >= 12) && (sugarStocks(machineID - 1) >= 5) && (milkStocks(machineID - 1) >= 200)){
      var prixfinal = prixLG + sucreP
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixLG, sucreP, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 6
      sugarStocks(machineID - 1) -= 5
      milkStocks(machineID - 1) -= 200
      return true
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 2) && (coffeeStocks(machineID - 1) >= 12) && (sugarStocks(machineID - 1) >= 10) && (milkStocks(machineID - 1) >= 200)){
      var prixfinal = prixLG + sucreM
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixLG, sucreM, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 12
      sugarStocks(machineID - 1) -= 10
      milkStocks(machineID - 1) -= 200
      return true
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 2) && (coffeeStocks(machineID - 1) >= 12) && (sugarStocks(machineID - 1) >= 15) && (milkStocks(machineID - 1) >= 200)){
      var prixfinal = prixLG + sucreB
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixLG, sucreB, prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 12
      sugarStocks(machineID - 1) -= 15
      milkStocks(machineID - 1) -= 200
      return true
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 1) && (coffeeStocks(machineID - 1) >= 12) && (milkStocks(machineID - 1) >= 200 + (50 * dose))){
      var prixfinal = prixLG + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f = CHF %.2f" + " \n", prixLG, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 12
      milkStocks(machineID - 1) -= 200 + ( 50 * dose)
      return true
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 1) && (coffeeStocks(machineID - 1) >= 12) && (sugarStocks(machineID - 1) >= 5) && (milkStocks(machineID - 1) >= 200 + (50 * dose))){
      var prixfinal = prixLG + sucreP + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixLG, sucreP, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 12
      sugarStocks(machineID - 1) -= 5
      milkStocks(machineID - 1) -= 200 + ( 50 * dose)
      return true
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 1) && (coffeeStocks(machineID - 1) >= 12) && (sugarStocks(machineID - 1) >= 10) && (milkStocks(machineID - 1) >= 200 + (50 * dose))){
      var prixfinal = prixLG + sucreM + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixLG, sucreM, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 12
      sugarStocks(machineID - 1) -= 10
      milkStocks(machineID - 1) -= 200 + ( 50 * dose)
      return true
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 1) && (coffeeStocks(machineID - 1) >= 12) && (sugarStocks(machineID - 1) >= 15) && (milkStocks(machineID - 1) >= 200 + (50 * dose))){
      var prixfinal = prixLG + sucreB + (prixLait * dose)
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n" + "Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f" + " \n" , prixLG, sucreB, (prixLait * dose), prixfinal)
      println("Veuillez payer en utilisant Twint \n" + "Votre code de paiement est : " + paiement + " \n" + "(En attente de validation du paiement...)" + " \n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté")
      println("Préparation de votre boisson... \n" + "Votre Latte (grand) est prêt ! Bonne dégustation !")
      coffeeStocks(machineID - 1) -= 12
      sugarStocks(machineID - 1) -= 15
      milkStocks(machineID - 1) -= 200 + ( 50 * dose)
      return true
    }

    // latte grand ( avec probleme de cafe)
    if((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 2) && (coffeeStocks(machineID - 1) < 12)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 2) && (coffeeStocks(machineID - 1) < 12)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 2) && (coffeeStocks(machineID - 1) < 12)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 2) && (coffeeStocks(machineID - 1) < 12)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 1) && (coffeeStocks(machineID - 1) < 12)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 1) && (coffeeStocks(machineID - 1) < 12)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 1) && (coffeeStocks(machineID - 1) < 12)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 1) && (coffeeStocks(machineID - 1) < 12)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }

    // Latte grand ( avec erreur de sucre)
    if((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 2) && (sugarStocks(machineID - 1) < 5)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 2) && (sugarStocks(machineID - 1) < 10)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 2) && (sugarStocks(machineID - 1) < 15)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 1) && (sugarStocks(machineID - 1) < 5)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 1) && (sugarStocks(machineID - 1) < 10)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 1) && (sugarStocks(machineID - 1) < 15)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }

    // Latte grand ( avec erreur de lait)
    if((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 2) && (milkStocks(machineID - 1) < 200)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 2) && (milkStocks(machineID - 1) < 200)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 2) && (milkStocks(machineID - 1) < 200)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 2) && (milkStocks(machineID - 1) < 200)){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Non \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 1) && (lait == 1) && (milkStocks(machineID - 1) < 200 + (50 * dose))){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Sans sucre \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if ((boisson == 3) && (latte == 3) && (sucre == 2) && (lait == 1) && (milkStocks(machineID - 1) < 200 + (50 * dose))){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Peu (5g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 3) && (lait == 1) && (milkStocks(machineID - 1) < 200 + (50 * dose))){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Moyen (10g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }
    else if((boisson == 3) && (latte == 3) && (sucre == 4) && (lait == 1) && (milkStocks(machineID - 1) < 200 + (50 * dose))){
      printf("Boisson sélectionnée : Latte (Grand) \n" + "Niveau de sucre : Beaucoup (15g) \n" + "Lait supplémentaire : Oui " + "(" + dose + " dose(s)" +") \n")
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. \n" + "Veuillez choisir une autre machine s'il vous plaît.")
      return false
    }

    else return false

  }
  def main(args: Array[String]): Unit = {

    var option = 0
    var mode = 0
    var boucle = 1
    var commande = true

    while( boucle == 1){
      // Lancement
      do{
        mode = readLine("        Nosepresso Café  \n" + "Veuillez sélectionner votre mode :  \n" + "1) Client  \n" + "2) Admin  \n" + "3) Quittez" + "\n").toInt
        if (mode == 1) {
          do{
             commande = serveClient(nbmachine, coffeeStocks, sugarStocks, milkStocks)
             }while(commande != true)

        }
        else if (mode == 2){
          var tentative = 2
          do{
            nbmachine = readLine("Machine sélectionnée (1-5) ? >").toInt
          }while(!(nbmachine == 1 || nbmachine == 2 || nbmachine == 3 || nbmachine == 4 || nbmachine == 5))
          mdp = readLine("Entrez le code PIN >")
          if(validatePin(nbmachine,code) != true ){
            do{
              tentative -= 1
              mdp = readLine("Code PIN incoreect. " + (tentative + 1 ) + " tentatives restantes.")
            }while(tentative != 0 && validatePin(nbmachine,code) != true)
          }
          if(validatePin(nbmachine,code) == true ){
            tentative += 1
            println("Accès à la machine " + nbmachine + " accordé")
            do{
               option = readLine("Que voulez-vous faire ? \n" + "1) Réapprovisionner le stock \n" + "2) Changer le PIN de la machine").toInt
            }while(!(option == 1 || option == 2))
            if(option == 1){
              println("Niveaux de stock actuels : \n" + "   Poudre de café : " + coffeeStocks(nbmachine -1) + " grammes"+ "\n" + "   Lait           : " + (milkStocks(nbmachine -1).toDouble / 1000 ) + " Litre"+ "\n" + "   Sucre          : " + sugarStocks(nbmachine -1) + " grammes \n")
              restockMachine(nbmachine, coffeeStocks, sugarStocks, milkStocks)
            }
            if(option == 2){
              updatePin(nbmachine,code)
            }
          }
          if( tentative == 0 ){
            println("Code PIN incorrect. 0 tentatives restantes. \n" + "Trop de tentatives échouées. Fin du programme.")
            boucle = 0
          }
        }
        else if (mode == 3){
          println("Merci ! Au revoir !")
          boucle = 0}
      }while (!(mode == 1 || mode == 2 || mode == 3))
    }
  }
}