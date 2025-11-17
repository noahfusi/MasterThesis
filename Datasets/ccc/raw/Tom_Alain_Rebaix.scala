import io.StdIn._
import collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.PrintWriter

class Machine(val id: Int, var pindcode: String, var milk: Int, var sugar: Int, var coffee: Int) {


  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "lait") {
      milk += amount
    }
    else if (ingredient == "sucre") {
      sugar += amount
    }
    else if (ingredient == "poudre de café")
      coffee += amount
  }
  def removeIngredient(ingredient: String, amount: Int): Boolean = {

    if (ingredient == "poudre de café") {
      if (coffee >= amount){
        coffee -= amount
        true
      }else {
        false
      }
    } else if (ingredient == "sucre") {
      if(sugar >= amount) {
        sugar -= amount
        true
      }else {
        false
      }
    } else if (ingredient == "lait" ) {
      if (milk >= amount) {
        milk -= amount
        true
      } else {
        false
      }
    }else {
      false
    }
  }

}

object Main {

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    val machines = ArrayBuffer[Machine]()
    try {
      val ma = Source.fromFile(filename)
      val lignema = ma.reset.getLines
      val premierligne = lignema.next()

      var i = 0


      while (lignema.hasNext) {
        var lignesmachines = lignema.next
        var uneMachine = lignesmachines.split (",")
        machines += new Machine(id = i + 1, uneMachine(0), uneMachine(1).toInt, uneMachine(2).toInt, uneMachine(3).toInt)
        i += 1
      }
      ma.close()
      println ("Chargement des machines depuis machines.csv... ")
      println( machines.size + " machines chargées avec succès")

      for(j<- 0 to (machines.size-1)){
        println ("\nmachine: " + machines(j).id + "\nmot-de-passe: " + machines(j).pindcode + "\nlait: " + machines(j).milk +"ml"+ "\nsucre: " + machines(j).sugar +"g"+  "\npoudre de café: " + machines(j).coffee +"g" )
      }

    }
    catch {
      case ex : java.io.FileNotFoundException => println("Fichier introuvable. Veuillez vérifier le chemin d'accès  ")
        println ("Fermeture du programme")
        sys.exit(0)
      case ex : java.io.IOException => println ("Erreur de sortie/ d'entrée")
        println ("Fermeture du programme")
        sys.exit(0)

    }
    println()
    machines
  }
  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {
      val fw = new PrintWriter(filename)
      fw.println("PINCODE " + " MILK " + " SUGAR " + "COFFEE ")
      for (machine <- machines) {
        fw.println(machine.pindcode + "," + machine.milk + "," + machine.sugar + "," + machine.coffee)
      }
      fw.close()
      println("sauvegarde de " + machines.size + " dans " + filename + "en cours...")
      println ("sauvegardé avec succès")
    }catch {
      case ex: java.io.FileNotFoundException => println ("Erreur dans l'écriture du fichier" + filename)
        println ("Fermeture du programme")
        sys.exit (0)
      case ex : java.io.IOException => println ("Erreur de sortie/ d'entrée")
        println ("Fermeture du programme")
        sys.exit(0)
      case ex : java.nio.file.AccessDeniedException => println ("Erreur dans l'écriture du fichier " + filename)
        println ("Fermeture du programme")
        sys.exit(0)

    }

  }

  def validatePin(machineId: Int, machinePins: ArrayBuffer[Machine]): Boolean = {
    var code = " "
    var essaie = 0
    var tentative = 2

    while (essaie < 3) {
      print(" Veuillez saisir le mot-de-passe de la machine :\n >")
      code = readLine()
      essaie += 1
      if (machinePins(machineId).pindcode == code) {
        println("mot-de-passe vailde")
        essaie = 4
        return true

      } else if (essaie < 3 && machinePins(machineId) != code) {
        println("code invalide veuillez réessayer. Tentative restante : " + tentative)
        tentative -= 1

      }
    }
    println("Maximun de tentaive atteint.")
    println("La machine est bloqué")
    sys.exit(0)
    return false
  }

  def updatePin(machineId: Int, machinePins: ArrayBuffer[Machine]): Unit = {
    var nouveaucode = " "
    val chiffre = Array('1', '2', '3', '4', '5', '6','7', '8','9','0')
    var changement = false

    println("Changement de mot-de-passe pour la machine " + (machineId + 1))

    while (!changement) {
      print("Entrez le nouveau mot-de-passe à 6 chiffres\n > ")
      nouveaucode = readLine()
      var tableaucode = nouveaucode.toCharArray
      var valide = true

      for(i <- 0 to nouveaucode.length -1 ){
        if(!chiffre.contains(tableaucode(i).toChar)){
          valide = false
        }
      }
      if (tableaucode.length == 6 && valide ) {
        println("Le mot-de-passe a été changé avec succès")
        machinePins(machineId).pindcode = nouveaucode
        changement = true
      } else {
        println("Le mot-de passe doit contenir 6 caractères ")
      }
    }

  }

  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var choix = 0
    var machine = machines(machineId)

    while (choix < 1 || choix > 3) {
      print("Veuillez sélectionner une boisson: \n1) Expresso - 2.00 CHF \n2) Cappucino - 2.50 CHF \n3) Latte - Petit 2.70 CHF, Moyen 3.20 CHF, Grand 3.70 CHF \n >")
      choix = readInt()
    }
    var laitboucle = false
    var choixlait = 0
    var lait = 0
    var sucre = 0
    var prixcafe = 0.0
    var choixlatte = 0
    var prix = 0.0
    var prixsucre = 0.0
    var prixlait = 0.0
    var aleatoire = 0
    if (choix == 3) {
      print("1) petit - 2.70 CHF  \n2) moyen - 3.20 CHF \n3) grand - 3.70 CHF \n >") // choix des différents latte
      choixlatte = readInt()
    }
    if (choix == 1) {
      laitboucle = true
    }
    while (sucre != 1 && sucre != 2 && sucre != 3 && sucre != 4) { // choix sucre
      print("Quel quantité de sucre :\n 1) pas de sucre\n 2) peu de sucre, 5g - 0.10 CHF  \n 3) moyen, 10g - 0.20 CHF \n 4) Beaucoup, 15g - 0.30 CHF \n >")
      sucre = readInt()
    }

    while (!laitboucle) {
      if (choix == 2 || choix == 3) {
        print("Vous voulez ajouter une dose supplémentaire de lait ? \n1) oui \n2) non \n >")
        choixlait = readInt()
      }
      if (choixlait == 1) {
        while (lait != 1 && lait != 2 && lait != 3) { // choix lait
          laitboucle = true
          print("Quel quantité de lait ? (maximun 3 doses)\n >")
          lait = readInt()
        }
      } else if (choixlait == 2) {
        laitboucle = true
      }
    }


    if (choix == 1) { // expresso
      laitboucle = true
      if (machine.removeIngredient("poudre de café", 8)) {
        prixcafe = 2.00
        println("Boisson sélectionée: Expresso")

      } else {
        println("Erreur: Quantité de poudre de café insuffisante \nVeuillez sélectionner une autre boisson ou contrôler le stock en mode admin\n")
        return false
      }
    } else if (choix == 2 && machine.removeIngredient("poudre de café", 6) && machine.removeIngredient("lait", 100)) { // choix cappucino
      println("Boisson sélectionée: Cappucino")
      prixcafe = 2.50

    } else if (machine.removeIngredient("poudre de café", 6) && choix == 2) {
      println("Erreur: Quantité de poudre de café insuffisante \nVeuillez sélectionner une autre boisson ou contrôler le stock en mode admin\n ")
      return false
    } else if (machine.removeIngredient("lait", 100) && choix == 2) {
      println("Erreur: Quantité de lait insuffisant \nVeuillez sélectionner une autre boisson ou contrôler le stock en mode admin\n ")
      return false
    } else if (choixlatte == 1 && machine.removeIngredient("poudre de café", 6) && machine.removeIngredient("lait", 120)) { //lattes
      prixcafe = 2.70
      println("Boisson sélectionée: Petit latte")
    }
    if (choixlatte == 2 && machine.removeIngredient("poudre de café", 8) && machine.removeIngredient("lait", 150)) {
      println("Boisson sélectionée: Moyen latte")
      prixcafe = 3.20
      prix = prixcafe
    }
    else if (choixlatte == 3 && machine.removeIngredient("poudre de café", 12) && machine.removeIngredient("lait", 200)) {
      prixcafe = 3.70
      println("Boisson sélectionée: Grand latte ")

    }
    else if ((choixlatte == 3 && machine.removeIngredient("poudre de café", 12)) || (choixlatte == 2 && machine.removeIngredient("poudre de café", 8)) || (choixlatte == 1 && machine.removeIngredient("poudre de café", 6))) {
      println("Erreur: Quantité de poudre de café insuffisante\n Veuillez sélectionner une autre boisson ou contrôler le stock en mode admin\n ")
      return false
    }
    else if ((choixlatte == 1 && machine.removeIngredient("lait", 120)) || (choixlatte == 2 && machine.removeIngredient("lait", 150)) || (choixlatte == 3 && machine.removeIngredient("lait", 200))) {
      println("Erreur: Quantité de lait insuffisant \nVeuillez sélectionner une autre boisson ou contrôler le stock en mode admin\n ")
      return false
    }


    if ((sucre == 4 && !machine.removeIngredient("sucre", 15)) || (sucre == 3 && !machine.removeIngredient("sucre", 10)) || (sucre == 2 && !machine.removeIngredient("sucre", 5))) {
      println("Erreur: Quantité de sucre insuffisante\nVeuillez sélectionner une plus petite quantité de sucre ou contrôler le stock en mode admin\n ")
      return false
    }
    if (sucre == 1) { // sucre
      println("Quantité de sucre : pas de sucre")
    }
    else if (sucre == 2 && machine.removeIngredient("sucre", 5)) {
      prixsucre = 0.10
      println("Quantité de sucre : peu (5g)")
    }
    else if (sucre == 3 && machine.removeIngredient("sucre", 10)) {
      prixsucre = 0.20
      println("Quantité de sucre : moyen (10g) ")
    }

    else if (sucre == 4 && machine.removeIngredient("sucre", 15)) {
      prixsucre = 0.30
      println("Quantité de sucre : beaucoup (15g)")
    }
    else if (choixlait == 2) { // lait
      println("Lait: pas de dose")
    }

    if (lait == 1 && machine.removeIngredient("lait", 50)) {
      prixlait = 0.05
      println("Lait: Une dose ")
    }

    else if (lait == 2 && machine.removeIngredient("lait", 100)) {
      prixlait = 0.1
      println("Lait: deux dose")
    }
    else if (lait == 3 && machine.removeIngredient("lait", 150)) {
      println("Lait: trois doses")
      prixlait = 0.15
    }
    else if ((lait == 3 && machine.removeIngredient("lait", 150)) || (lait == 2 && machine.removeIngredient("lait", 100)) || (lait == 1 && machine.removeIngredient("lait", 50))) {
      println("Erreur: Quantité lait insuffisante pour la dose sélectionné \nVeuillez sélectionner une plus petite quantité de dose de lait ou contrôler le stock en mode admin\n ")
      return false
    }

    if (prixcafe > 0.00 || prixlait > 0.00 || prixsucre > 0.00) { // le prix
      prix = prixcafe + prixlait + prixsucre
    }
    if (prixsucre > 0.00 && prixlait > 0.00) {
      printf("Le prix de la boisson est de : %.2f CHF = %.2f CHF (café) + %.2f CHF (sucre) + %.2f CHF (lait)\n", prix, prixcafe, prixsucre, prixlait)
    }
    if (prixsucre > 0.00 && prixlait == 0.00) {
      printf("Le prix de la boisson est de : %.2f CHF = %.2f CHF (café) + %.2f CHF (sucre) \n", prix, prixcafe, prixsucre)
    }
    if (prixsucre == 0.00 && prixlait > 0.00) {
      printf("Le prix de la boisson est de : %.2f CHF = %.2f CHF (café) + %.2f CHF (lait) \n", prix, prixcafe, prixlait)
    }
    if (prixsucre == 0.00 && prixlait == 0.00) {
      printf("Le prix de la boisson est de : %.2f CHF (café) \n", prixcafe)
    }

    println("\nVeuillez procéder au paiement avec Twint")
    print("Votre code pour le paiment est : ")
    for (i <- 0 to 4) {
      while ((aleatoire < 49) || (aleatoire > 90) || ((aleatoire < 65) && (aleatoire > 57))) {
        aleatoire = (Math.random() * 90).toInt
      }
      print(aleatoire.toChar)
      aleatoire = 0
    }
    println("\nEn attente de la confirmation du paiement...")
    Thread.sleep(3000)
    println("Paiement validé ! Merci pour votre commande")
    println("\nVotre commande est cour de préparation...")

    if (choix == 1) {
      println("Voici votre expresso ! ")
      return true
    }
    if (choix == 2) {
      println("Voici votre cappuccino !")
      return true
    }
    if (choix == 3) {
      println("Voici votre latte !")
      return true

    }
    else {
      return false
    }
  }

  def restockMachine(machineId: Int, machine: ArrayBuffer[Machine]): Unit = {

    var poudreajoute = -1
    while (poudreajoute < 0) {
      print(" Quel quantité de poudre à café à rajouter \n >")
      poudreajoute = readInt()
    }
    machine(machineId).addIngredient("poudre de café (en gramme)", poudreajoute)

    var sucreajoute = -1
    while (sucreajoute < 0) {
      print(" Quel quantité de sucre à rajouter (en gramme) \n >")
      sucreajoute = readInt()
    }
    machine(machineId).addIngredient("sucre", sucreajoute)

    var laitajoute = -1.0
    while (laitajoute < 0) {
      print(" Quel quantité de lait à rajouter (en litre) \n >")
      laitajoute = readDouble() * 1000.0

    }
    var laitajoute2 = (laitajoute).toInt
    machine(machineId).addIngredient("lait", laitajoute2)
    println("réapprovisionnement des stocks ...")
    println("Quantités de stock:\n1) Poudre de café: " + machine(machineId).coffee + "g \n2) Sucre: " + machine(machineId).sugar + "g\n3) Lait: " + machine(machineId).milk / 1000.0 + "L")

  }

  def main(args: Array[String]): Unit = {
    var entre = 0
    val machines = loadcsv("machines.csv")
    var codemachines = Array.fill(5)("434343")

    while (entre != 1 && entre != 2 && entre != 3) {

      print("\t Nospresso café \n Veuillez séletcioner votre mode \n 1) Client \n 2) Admin \n 3) Quitter \n>")
      entre = readInt()
    }

    while (entre == 1 || entre == 2) {
      var choixmachine = false
      var nbrmachines = 0
      var chagementmdp = 0
      if (entre == 1) {
        while (!choixmachine) {
          print("Veuillez séléectionner une machine entre 1 et 5  \n >")

          nbrmachines = readInt()
          if (nbrmachines <= 5 && nbrmachines >= 1) {
            nbrmachines -= 1
            choixmachine = true
          } else {
            println("saisie non-valide")
          }
        }
        serveClient(nbrmachines, machines)
        print("\n\t Nospresso café \n Veuillez séletcioner votre mode \n 1) Client \n 2) Admin \n 3) Quitter \n >")
        entre = readInt()
        choixmachine = false
      }

      else if (entre == 2) {
        while (!choixmachine) {
          print("Veuillez séléectionner une machine entre 1 et 5  \n >")
          nbrmachines = readInt() - 1
          if (nbrmachines <= 4 && nbrmachines >= 0) {
            choixmachine = true
          } else {
            println("saisie non-valide")
          }
        }

        validatePin(nbrmachines, machines)

        while (chagementmdp <= 0 || chagementmdp > 2) {
          print("Que voulez-vous faire ? \n1) Changer de mot-de-passe \n2) Vérifier les stocks \n>")
          chagementmdp = readInt()
        }
        if (chagementmdp == 1) {
          updatePin(nbrmachines, machines)
        } else {
          restockMachine(nbrmachines, machines)
        }
        println("Retour au meunu...  ")
        print("\n\t Nospresso café \n Veuillez séletcioner votre mode \n 1) Client \n 2) Admin \n 3) Quitter \n >")
        entre = readInt()
      }
      savecsv("machines.csv", machines)

      while (entre != 1 && entre != 2 && entre != 3) {
        println("Sasie non-valide, veuillez réessayer \n ")
        print("\t Nospresso café \n Veuillez séletcioner votre mode \n 1) Client \n 2) Admin \n 3) Quitter \n>")
        entre = readInt()
      }
    }
  }
}
