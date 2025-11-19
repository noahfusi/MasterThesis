import scala.io.StdIn.readLine
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.PrintWriter

//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or
// click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
object Exercice3 {

  class Machine(val id: Int, var pincode: String, var milk: Int,
                var sugar: Int, var coffee: Int) {


    def addIngredient(ingredient: String, amount: Int): Unit = {
      if (ingredient == "coffee") {
        coffee += amount
      } else if (ingredient == "milk") {
        milk += amount
      } else if (ingredient == "sugar") {
        sugar += amount
      }
    }

    def removeIngredient(ingredient: String, amount: Int): Boolean = {
      if ((ingredient == "coffee") & ((coffee - amount) >= 0)) {
        coffee -= amount
        return true
      } else if ((ingredient == "milk") & ((milk - amount) >= 0)) {
        milk += amount
        return true
      } else if ((ingredient == "sugar") & ((sugar - amount) >= 0)) {
        sugar += amount
        return true
      } else return false
    }
  }

  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {

    var code_pin = ""
    var result = true

    code_pin = readLine("Entrez le code PIN : \n")

    if (code_pin == machines(machineId).pincode) {
      result = true
    } else {
      result = false
    }
    return result
  }


  def updatePin(machineId: Int,  machines: ArrayBuffer[Machine]): Unit = {

    var nv_code_pin = ""

    println("Mise à jour du code PIN pour la machine " + (machineId+1) + ".")

    do {
      nv_code_pin = readLine("\nEntrez un nouveau code PIN à 6 chiffres:  ")

    } while ((nv_code_pin.length() != 6) || (!nv_code_pin.forall(_.isDigit)))

    machines(machineId).pincode = nv_code_pin

    println("\nLe code PIN a été mis à jour avec succès.\nRetour au menu principal...\n\n")
  }


  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {

    var result = true



    var res_prix = 0.00
    var prix_lait_sup = 0.00
    var erreur = false



    val achat_expresso_cafe = 8
    val achat_cappuccino_cafe = 6
    val achat_latte_petit_cafe = 6
    val achat_latte_moyen_cafe = 8
    val achat_latte_grand_cafe = 12

    val achat_cappuccino_lait = 100
    val achat_latte_petit_lait = 120
    val achat_latte_moyen_lait = 150
    val achat_latte_grand_lait = 200

    val sucre_peu = 5
    val sucre_moyen = 10
    val sucre_beaucoup = 15

    val dose_lait = 50



    val prix_expresso = 2.00
    val prix_cappuccino = 2.50
    val prix_latte_petit = 2.70
    val prix_latte_moyen = 3.20
    val prix_latte_grand = 3.70

    val prix_sucre_peu = 0.10
    val prix_sucre_moyen = 0.20
    val prix_sucre_beaucoup = 0.30

    val prix_dose_lait = 0.05


    var res_cafe = 0
    var res_sucre = 0
    var res_lait = 0


    // Pour afficher les bons prix
    var prix_boisson_simple = 0.00


    var boisson = 0
    var sucre = 0
    var lait = 0
    var nbr_de_doses = 0

    // Les queries à l'utilisateur
    do {
      boisson = readLine("\nVeuillez sélectionner votre boisson: \n1) Expresso - CHF 2.00 \n2) Cappuccino - CHF 2.50" +
        "\n3) Latte - CHF 2.70 (Petit)\n4) Latte - CHF 3.20 (Moyen)\n5) Latte - CHF 3.70 (Grand)\n").toInt

      if ((boisson != 1) && (boisson != 2) && (boisson != 3) &&
        (boisson != 4) && (boisson != 5)) {
        println("Veuillez choisir une boisson existante.")
      }
    } while ((boisson != 1) && (boisson != 2) && (boisson != 3) &&
      (boisson != 4) && (boisson != 5))


    do {
      sucre = readLine("\nSouhaitez-vous ajouter du sucre ? \n1) Sans sucre \n2) Peu (5g)- CHF 0.10 \n3) Moyen (10g)- CHF 0.20 \n4) Beaucoup (15g)- CHF 0.30\n").toInt

    } while ((sucre != 1) && (sucre != 2) && (sucre != 3) && (sucre != 4))


    if ((boisson == 2) || (boisson == 3) || (boisson == 4) || (boisson == 5)) {
      do {
        lait = readLine("\nSouhaitez-vous ajouter du lait en supplément ?\n1) Oui \n2) Non\n").toInt
      } while ((lait != 1) && (lait != 2))
    }

    if (lait == 1) {
      do {
        nbr_de_doses = readLine("\nCombien de dose ?\n").toInt
      } while ((nbr_de_doses < 1) || (nbr_de_doses > 3))
    }

    // On fait les calculs des quantités pour l'achat de la boisson
    if (boisson == 1) {

      var remove = true
      remove = machines(machineId).removeIngredient("coffee", achat_expresso_cafe)

      res_lait = machines(machineId).milk
      res_prix = res_prix + prix_expresso
      prix_boisson_simple = prix_expresso
      println("\nBoisson sélectionnée : Expresso")

    } else if (boisson == 2) {
      var remove = true
      remove = machines(machineId).removeIngredient("coffee", achat_cappuccino_cafe)
      var remove2 = true
      remove2 = machines(machineId).removeIngredient("milk", achat_cappuccino_lait)

      res_prix = res_prix + prix_cappuccino
      prix_boisson_simple = prix_cappuccino
      println("\nBoisson sélectionnée : Cappuccino")

    } else if (boisson == 3) {
      var remove = true
      remove = machines(machineId).removeIngredient("coffee", achat_latte_petit_cafe)
      var remove2 = true
      remove2 = machines(machineId).removeIngredient("milk", achat_latte_petit_lait)

      res_prix = res_prix + prix_latte_petit
      prix_boisson_simple = prix_latte_petit
      println("\nBoisson sélectionnée : Latte (Petit)")

    } else if (boisson == 4) {
      var remove = true
      remove = machines(machineId).removeIngredient("coffee", achat_latte_moyen_cafe)
      var remove2 = true
      remove2 = machines(machineId).removeIngredient("milk", achat_latte_moyen_lait)

      res_prix = res_prix + prix_latte_moyen
      prix_boisson_simple = prix_latte_moyen
      println("\nBoisson sélectionnée : Latte (Moyen)")

    } else if (boisson == 5) {
      var remove = true
      remove = machines(machineId).removeIngredient("coffee", achat_latte_grand_cafe)
      var remove2 = true
      remove2 = machines(machineId).removeIngredient("milk", achat_latte_grand_lait)

      res_prix = res_prix + prix_latte_grand
      prix_boisson_simple = prix_latte_grand
      println("\nBoisson sélectionnée : Latte (Grand)")
    }

    // Les calculs du sucre
    if (sucre == 2) {
      var remove = true
      remove = machines(machineId).removeIngredient("sugar", sucre_peu)

      res_prix = res_prix + prix_sucre_peu
      println("Niveau de sucre : Peu (5g)")

    } else if (sucre == 3) {
      var remove = true
      remove = machines(machineId).removeIngredient("sugar", sucre_moyen)

      res_prix = res_prix + prix_sucre_moyen
      println("Niveau de sucre : Moyen (10g)")

    } else if (sucre == 4) {
      var remove = true
      remove = machines(machineId).removeIngredient("sugar", sucre_beaucoup)

      res_prix = res_prix + prix_sucre_beaucoup
      println("Niveau de sucre : Beaucoup (15g)")

    } else if (sucre == 1) {
      res_sucre = machines(machineId).sugar// Pour garder le même niveau de sucre
      println("Niveau de sucre : Sans sucre")
    }

    // Les calculs du lait
    if (lait == 1) {
      var remove2 = true
      remove2 = machines(machineId).removeIngredient("milk", (nbr_de_doses * dose_lait))

      res_prix = res_prix + (nbr_de_doses * prix_dose_lait)
      prix_lait_sup = nbr_de_doses * prix_dose_lait
      println("Lait supplémentaire: Oui\nNombres de doses: " + nbr_de_doses)

    } else if (lait == 2) {
      println("Lait supplémentaire: Non")
    }





    // Les messages d'erreurs personnalisés en fonction du manque de café
    if (((boisson == 2) || (boisson == 3)) && (res_cafe < 0)) {
      println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez vérifier les stocks en mode Admin.\n")
      erreur = true

    } else if ((boisson == 1) && (res_cafe < 0)) {
      println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les stocks en mode Admin.\n")
      erreur = true

    } else if (((boisson == 4) || (boisson == 5)) && (res_cafe < 0)) {
      println("\nErreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")
      erreur = true


      // Les messages d'erreurs personnalisés en fonction du manque de lait
    } else if ((boisson == 2) && (res_lait < 0) && !erreur) {
      if (lait == 1) {
        println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez prendre moins de lait ou sélectionner une autre boisson.\n")
        erreur = true
      } else {
        println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez sélectionner une autre boisson ou vérifier les stocks en mode Admin.\n")
        erreur = true
      }

    } else if ((boisson == 3) && (res_lait < 0) && !erreur) {
      if (lait == 1) {
        println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez prendre moins de lait ou sélectionner une autre boisson.\n")
        erreur = true
      } else {
        println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez sélectionner une autre boisson ou vérifier les stocks en mode Admin.\n")
        erreur = true
      }

    } else if (((boisson == 4) || (boisson == 5)) && (res_lait < 0) && !erreur) {
      if (lait == 1) {
        println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez prendre moins de lait ou sélectionner une autre boisson.\n")
        erreur = true
      } else {
        println("\nErreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer une autre boisson.\n")
        erreur = true
      }



      // Les messages d'erreurs personnalisés en fonction du manque de sucre
    } else if ((res_sucre < 0) && !erreur) {
      println("\nErreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.\nVeuillez prendre moins de sucre ou ou vérifier les stocks en mode Admin.\n")
      erreur = true
    }




    // Pour afficher les prix
    if (!erreur) {
      if ((sucre == 1) && ((lait == 2)) || (lait == 0)){
        printf("Prix total : CHF %.2f", res_prix)

      } else if ((sucre == 1) && (lait == 1)) {
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prix_boisson_simple, prix_lait_sup, res_prix)

      } else if ((sucre == 2) && ((lait == 2) || (lait == 0))) {
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prix_boisson_simple, prix_sucre_peu, res_prix)

      } else if ((sucre == 3) && ((lait == 2)|| (lait == 0))) {
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prix_boisson_simple, prix_sucre_moyen, res_prix)

      } else if ((sucre == 4) && ((lait == 2)|| (lait == 0))) {
        printf("Prix total : CHF %.2f + CHF %.2f = CHF %.2f", prix_boisson_simple, prix_sucre_beaucoup, res_prix)

      } else if ((sucre == 2) && (lait == 1)) {
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prix_boisson_simple, prix_lait_sup, prix_sucre_peu, res_prix)

      } else if ((sucre == 3) && (lait == 1)) {
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prix_boisson_simple, prix_lait_sup, prix_sucre_moyen, res_prix)

      } else if ((sucre == 4) && (lait == 1)) {
        printf("Prix total : CHF %.2f + CHF %.2f + CHF %.2f = CHF %.2f", prix_boisson_simple, prix_lait_sup, prix_sucre_beaucoup, res_prix)
      }





      // generer un code twint avec util.random : est ce qu'on a le droit ? + afficher le message

      println("\n\nVeuillez payer en utilisant Twint.\nVotre code de paiement est : " + new String(Random.alphanumeric.take(5).toArray) + "\n(En attente de paiement...)\n")
      Thread.sleep(3000)
      println("Merci ! Votre paiement a été accepté.")
      println("Paiement confirmé.\nPréparation de votre boisson... \nVotre boisson est prête ! Bonne dégustation !")






    }


    if (erreur) {
      return (!result)
    } else {
      return result
    }

  }


  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {

    // Pour ajouter en mode admin
    var ajout_cafe = 0
    var ajout_lait = 0
    var ajout_sucre = 0



    println("Stocks:\n    Poudre de café: " + machines(machineId).coffee + "g\n    Lait          : " + machines(machineId).milk.toDouble/1000 + "L\n    Sucre         : " + machines(machineId).sugar + "g\n")


    do {
      ajout_cafe = readLine("Veuillez indiquer la quantité de poudre à café à rajouter en grammes:\n").toInt

    } while (ajout_cafe < 0)

    do {
      ajout_lait = readLine("Veuillez indiquer la quantité de lait à rajouter en millilitres:\n").toInt

    } while (ajout_lait < 0)

    do {
      ajout_sucre = readLine("Veuillez indiquer la quantité de sucre à rajouter en grammes:\n").toInt

    } while (ajout_sucre < 0)

    println("\nRéapprovisionnement des stocks...")
    println("Ajout :\n    Poudre de café: " + ajout_cafe + "g\n    Lait          : " + (ajout_lait.toDouble/1000) + "L\n    Sucre         : " + ajout_sucre + "g\n")

    // On mets à jour les stocks
    machines(machineId).addIngredient("coffee", ajout_cafe)
    machines(machineId).addIngredient("sugar", ajout_sucre)
    machines(machineId).addIngredient("milk", ajout_lait)


    println("Niveaux de stock mis à jour.\nRetour au menu principal...")
  }



  def loadcsv(filename: String): ArrayBuffer[Machine] = {

    try {

      val fr = Source.fromFile(filename)
      val lignefr = fr.reset.getLines.drop(1)
      val machines = new ArrayBuffer[Machine]()
      var i = 0

      while (!lignefr.isEmpty) {
        var ligne = lignefr.next
        var machine = ligne.split(",")
        machines += new Machine(i + 1, machine(0), machine(1).toInt, machine(2).toInt, machine(3).toInt)

        i += 1
      }
      return machines
    }
    catch {
      case ex : java.io.FileNotFoundException => println("Fichier n'existe pas")
        return null
    }
  }




  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    try {

      val fw = new PrintWriter(filename)

      for (i <- 0 until machines.length) {

        fw.println(machines(i).pincode + ";" + machines(i).milk + ";" + machines(i).sugar + ";" + machines(i).coffee)
      }
      fw.close()
    }
    catch {
      case ex : java.io.FileNotFoundException => println("Fichier n'existe pas")
        return null
    }
  }













  // La méthode main commence ici: voir les grammes de lait/ les verifications d'entrees /
  def main(args: Array[String]): Unit = {

    var onoff = true



    println("Chargement des machines depuis machines.csv...\n\n")
    var machines = new ArrayBuffer[Machine]()
    machines = loadcsv("machines.csv")
    var nbr = 0

    if (machines != null) {
      for(i <- 0 to machines.length - 1) {
        println("Machine" + i + " chargée :\n    ID: " + i + "\n    Code PIN: " + machines(i).pincode + "\n    Lait: " + machines(i).milk + "\n    Sucre: " + machines(i).sugar + "\n    Café: " + machines(i).coffee + "\n\n")

        nbr += 1
      }
      println(nbr + "machine(s) chargée(s) avec succès.\n\n")
    }




    while (onoff) {
      var mode = 0
      do {
        mode = readLine("\n        Nospresso Café \nVeuillez sélectionner votre mode : \n1) Client \n2) Admin \n3) Quitter\n").toInt

      } while ((mode != 1) && (mode != 2) && (mode != 3))


      if (mode == 1) {
        var nbr_machine = 0
        var etat_achat = true



        do {
          nbr_machine = (readLine("\nMachine sélectionnée  :").toInt)-1

        } while ((nbr_machine < 0) || (nbr_machine > machines.length-1))

        etat_achat = serveClient(nbr_machine,machines)

        if (!etat_achat) {
          println("Veuillez essayer une autre machine pour votre commande.\n")
        }

      } else if (mode == 2){

        var nbr_machine = 0
        var etat_pin = true
        var nbr_essais = 0
        var action_admin = 0


        do {
          nbr_machine = (readLine("\nMachine sélectionnée  :").toInt)-1

        } while ((nbr_machine < 0) || (nbr_machine > machines.length-1))



        do {

          nbr_essais += 1

          etat_pin = validatePin(nbr_machine, machines)
          if (!etat_pin & (nbr_essais == 1)) {
            println("Code PIN incorrect. 2 tentatives restantes.\n")
          } else if (!etat_pin & (nbr_essais == 2)) {
            println("Code PIN incorrect. 1 tentative restante.\n")
          } else if (!etat_pin & (nbr_essais == 3)) {
            println("Code PIN incorrect. 0 tentatives restantes.\n\nTrop de tentatives échouées. Fin du programme.\n")
            onoff = false

          }
        } while (!etat_pin & (nbr_essais <3 ))



        if (etat_pin) {
          println("Accès accordé à la machine " + (nbr_machine+1) + ".\n")

          do {
            action_admin = readLine("Choisissez votre action en mode administrateur:\n1) Mettre à jour le code PIN\n2) Mettre à jour les stocks\n\n").toInt
          } while ((action_admin != 1) & (action_admin != 2))

          if (action_admin == 1) {
            updatePin(nbr_machine, machines)
          } else if (action_admin == 2){
            restockMachine(nbr_machine, machines)

          }

        }




      } else if (mode == 3){
        onoff = false
        savecsv("machines.csv", machines)
        println("Sauvegarde de " + nbr + "machines dans machines.csv...\n Fichier sauvegardé avec succès.")

      }

    }
  }
}
