import scala.util.Random

object Main {
  val NOMBRE_MACHINE = 5
  var q_cafe: Array[Int] = Array.fill(NOMBRE_MACHINE) { 50 }
  var q_sucre: Array[Int] = Array.fill(NOMBRE_MACHINE) { 30 }
  var q_lait: Array[Int] = Array.fill(NOMBRE_MACHINE) { 500 } // ml
  var pins: Array[String] = Array.fill(NOMBRE_MACHINE) { "434343" }

  def main(args: Array[String]): Unit = {
    var quit = false

    while(!quit){
      println("	Nospresso Café")
      println("Veuillez sélectionner une option :\n1) Client\n2) Admin\n3) Quitter")
      val choix = choose(1, 3)
      if(choix == 1){
        print("Machine sélectionnée (1-5) ")
        var machineId = choose(1, NOMBRE_MACHINE)-1
        while(!serveClient(machineId, q_cafe, q_sucre, q_lait)){
          println(s"Veuillez sélectionner une autre machine ou entrez ${NOMBRE_MACHINE+1} pour retourner au menu.")
          machineId = choose(1, NOMBRE_MACHINE+1)-1
        }
      }
      else if(choix == 2){
        if(!admin(pins, q_cafe, q_sucre, q_lait)){
          quit = true
        }
      }
      else if(choix == 3){
        quit = true
      }
    }

  }

  def choose(lowRange: Int, highRange: Int): Int = {
    var choix_mode = -1
    while ((choix_mode < lowRange) || (choix_mode > highRange)) {
      choix_mode = scala.io.StdIn.readInt()
      if ((choix_mode < lowRange) || (choix_mode > highRange)){
        println(s"Veuillez entrer un nombre entre $lowRange et $highRange.")
      }
    }
    return choix_mode
  }

  def calculatePrice(choix_cafe: Int, choix_sucre: Int, choix_lait: Int): Array[Double] = {
    val price_cafe = Array(2, 2.5, 0.0, 2.7, 3.2, 3.7)
    return Array(price_cafe(choix_cafe-1), (choix_sucre-1) * 0.10, (choix_lait-2) * 0.05)
  }

  def makeDrinkStrings(choix_cafe: Int, choix_sucre: Int, choix_lait: Int): Array[String] = {
    val cafe_types = Array("Expresso", "Capuccino", "", "Latte (Petit)", "Latte (Moyen)", "Latte (Grand)")
    val sucre_types = Array("Sans sucre", "Peu (5g)", "Moyen (10g)", "Beaucoup (15g)")
    val lait_types = Array("Non", "Une dose", "Deux doses", "Trois doses")
    var boisson = cafe_types(choix_cafe-1)
    var sucre = sucre_types(choix_sucre-1)
    var lait = lait_types(choix_lait-2)
    return Array(boisson, sucre, lait)
  }

  def calculateConso(choix_cafe: Int, choix_sucre: Int, choix_lait: Int): Array[Int] = {
    val consos_cafe = Array(8, 6, 0, 6, 8, 12)
    val consos_lait = Array(0, 100, 0, 120, 150, 200)
    return Array(consos_cafe(choix_cafe-1), (choix_sucre-1)*5, (choix_lait-2)*50 + consos_lait(choix_cafe-1))
  }

  def checkIngredients(conso: Array[Int], machine: Int): Boolean = {
    if(conso(0) > q_cafe(machine)){
      return false
    }
    if(conso(1) > q_sucre(machine)){
      return false
    }
    if(conso(2) > q_lait(machine)){
      return false
    }
    return true
  }

  def serveClient(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    if(machineId >= NOMBRE_MACHINE){
      return true;
    }
    println("Veuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Cappuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    var choix_cafe = choose(1, 3)
    if(choix_cafe == 3){
      println("Veuillez sélectionner la taille de votre Latte :\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHF 3.70")
      val choix_latte = choose(1, 3)
      choix_cafe += choix_latte
    }
    println("Souhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30")
    var choix_sucre = choose(1, 4)
    var choix_lait = 2
    if (choix_cafe != 1) {
      println("Souhaitez-vous ajouter du lait en supplément ?\n1) Oui\n2) Non")
      choix_lait = choose(1, 2)
      if(choix_lait == 1){
        println("Combien de dose ?")
        choix_lait = choose(1, 3)+2
      }
    }
    val price = calculatePrice(choix_cafe, choix_sucre, choix_lait)
    val drinks = makeDrinkStrings(choix_cafe, choix_sucre, choix_lait)
    val conso = calculateConso(choix_cafe, choix_sucre, choix_lait)

    println("Boisson sélectionnée : " + drinks(0))
    println("Niveau de sucre : " + drinks(1))
    println("Lait supplémentaire : " + drinks(2))

    if(checkIngredients(conso, machineId)){
      print("Prix total : ")
      printf("CHF %.2f", price(0))
      if (price(1) > 0)
        printf(" + CHF %.2f", price(1))
      if (price(2) > 0)
        printf(" + CHF %.2f", price(2))
      printf(" = CHF %.2f\n", price(0)+price(1)+price(2))
      println("Veuillez payer en utilisant Twint.")
      println(s"Votre code de paiement est : " + Random.alphanumeric.take(5).mkString(""))
      println("(En attente de validation du paiement...)")
      Thread.sleep(3000)
      println("\nMerci ! Votre paiement a été accepté.")
      println("\nPréparation de votre boisson...")
      Thread.sleep(5000)
      println("Votre " + drinks(0) + " est prêt ! Bonne dégustation !\n")
      coffeeStocks(machineId) -= conso(0)
      sugarStocks(machineId) -= conso(1)
      milkStocks(machineId) -= conso(2)
      return true
    }
    if(conso(0) > q_cafe(machineId)) {
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
    }
    if(conso(1) > q_sucre(machineId)) {
      println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
    }
    if(conso(2) > q_lait(machineId)) {
      println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
    }
    return false
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    val code = scala.io.StdIn.readLine()
    if(code == machinePins(machineId)){
      return true
    }
    return false
  }

  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    while(true){
      print(s"Entrez un nouveau code PIN pour la machine ${machineId+1}: ")
      val code = scala.io.StdIn.readLine()
      if(code.length == 6){
        println("Le code PIN a été mis à jour avec succès.")
        println("Retour au menu principal...")
        machinePins(machineId) = code
        return
      }
      else{
        println("Nouveau code PIN non-valide.")
        println("Réessayez avec un code à 6 chiffres.")
      }
    }
  }

  def restockMachine(machineId: Int, coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Unit = {
    println("Niveaux de stock actuels :")
    println(s"   Poudre à café : ${coffeeStocks(machineId)}g")
    println(s"   Sucre : ${sugarStocks(machineId)}g")
    println(s"   Lait : ${milkStocks(machineId).toDouble/1000.0}L")

    print("Quantité de poudre à café à ajouter (g): ")
    val quantCafe = scala.io.StdIn.readInt()
    if(quantCafe > 0){
      coffeeStocks(machineId) += quantCafe
    }
    print("Quantité de sucre à ajouter (g): ")
    val quantSucre = scala.io.StdIn.readInt()
    if(quantSucre > 0){
      sugarStocks(machineId) += quantSucre
    }
    print("Quantité de lait à ajouter (mL): ")
    val quantLait = scala.io.StdIn.readInt()
    if(quantLait > 0){
      milkStocks(machineId) += quantLait
    }

    println("Niveaux de stocks modifiés:")
    println(s"   Poudre à café : ${coffeeStocks(machineId)}g")
    println(s"   Sucre : ${sugarStocks(machineId)}g")
    println(s"   Lait : ${milkStocks(machineId).toDouble/1000.0}L")
  }

  def admin(machinePins: Array[String], coffeeStocks: Array[Int], sugarStocks: Array[Int], milkStocks: Array[Int]): Boolean = {
    print("Machine sélectionnée (1-5) ")
    var machineId = choose(1, NOMBRE_MACHINE)-1
    println("Entrez le code PIN: ")
    var iteration = 0
    while(!validatePin(machineId, machinePins) && iteration < 3){
      if(iteration >= 2){
        println("Trop de tentatives échouées. Fin du programme.")
        return false
      }
      var tentative = " tentative restante."
      if(iteration != 1){
        tentative = " tentatives restantes."
      }
      println(s"Code PIN incorrect. ${2-iteration}"+tentative)
      iteration += 1
    }
    print("Machine à modifier (1-5) ")
    var newMachineId = choose(1, NOMBRE_MACHINE)-1
    println("1) Réapprovisionner\n2) Modifier le code PIN\n3) Retourner au menu principal")
    var choice = choose(1, 3)
    if(choice == 1){
      restockMachine(newMachineId, coffeeStocks, sugarStocks, milkStocks)
    }
    else if(choice == 2){
      updatePin(newMachineId, machinePins)
    }
    return true
  }

}
