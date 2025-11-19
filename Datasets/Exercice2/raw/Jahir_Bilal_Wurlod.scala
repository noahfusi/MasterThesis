import scala.io.StdIn.readLine
import io.StdIn._
import scala.math._
import scala.io.StdIn.readInt


object Main {

  def validatePIN(Nummachine: Int, PINmachine: Array[Int]): Boolean = {

    var PIN2 = 98989898
    var tryPIN = 0

    while (!(PIN2 == PINmachine(Nummachine) || tryPIN == 3)) {
      tryPIN += 1
      println("essaye NB : " + tryPIN +" sur 3")
      println("entrer le Pin :")
      print("> ")
      PIN2 = readLine().toInt
      if (!(PIN2 == PINmachine(Nummachine))) println("faux")

      if ((PIN2 == PINmachine(Nummachine))) {
        println("Admin confirmé Pour la machine ZONE > "+Nummachine)
        return true
      }
      if (tryPIN == 3) {
        println("Nombre d'eesays épuisé.")
        return false
      }
    }
    return false
  }





  // ------------------------------    CHANGE    PIN      --------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------


  def UpdatePIN(Num2machine: Int, PIN2machine: Array[Int]): Unit = {
    val chiffrePINMAX = 999999
    val chiffrePINMIN = 100000
    var chiffrePINutiliser = 0

    while (chiffrePINutiliser >= chiffrePINMAX || chiffrePINutiliser <= chiffrePINMIN) {
      println("entrer le NEW CODE : ")
      chiffrePINutiliser = readInt()
      if ((chiffrePINutiliser >= chiffrePINMAX) || (chiffrePINutiliser <= chiffrePINMIN)) {
        println("entrer un CODE a 6 chiffre")
      } else {

        PIN2machine(Num2machine)= chiffrePINutiliser
        println("CONFIRMER")
        println("CODE des MACHINE des ZONE 0 a 4 : "+ PIN2machine.mkString(","))
        println("MACHINE doit etre réinitialiser, RETOURNE au MENU PRINCIPALE")



      }
    }


  }





  // -----------------------------------   NEW     STOCK    -------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------


  def restockmachine(Num3machine: Int, stockcafe: Array[Int], stocklait: Array[Double], stocksucre: Array[Int]): Unit = {

    var reaprovisation = 0
    var caferajouter = 0
    var laitrajouter = 0.0
    var sucrerajouter = 0
    println("Stocks de la machine " + (Num3machine + 1) + " :")


    println("poudre a café : " + stockcafe(Num3machine) + "g")
    println("Lait : " + stocklait(Num3machine) + "L")
    println("Sucre : " + stocksucre(Num3machine) + "g")


    while (!(reaprovisation == 1 || reaprovisation == 2 || reaprovisation == 3)) {

      reaprovisation = 0
      caferajouter = 0
      laitrajouter = 0.0
      sucrerajouter = 0


      println("Réapprovisionnement des stocks... Ajout :")
      println("1) café")
      println("2) Lait")
      println("3) Sucre")
      reaprovisation = readLine().toInt

      if (!(reaprovisation == 1) && !(reaprovisation == 2) && !(reaprovisation == 3) && !(reaprovisation == 4)) println("Tapé 1, 2 ou 3")


    }

    if (reaprovisation == 1) {
      caferajouter = -1
      while (caferajouter <= (-1)) {
        caferajouter = 0
        println("quantité de café rajouté : ")
        caferajouter = readLine().toInt
        stockcafe(Num3machine) += caferajouter

        if (caferajouter < 0) {
          println("quantité non valide, entrer nombre positif.")
          stockcafe(Num3machine) -= caferajouter
        }

      }

    } else {
      if (reaprovisation == 2) {
        laitrajouter = -0.1
        while (laitrajouter <= (-0.0000001)) {
          laitrajouter = 0
          println("quantité de Lait rajouté (en L): ")
          laitrajouter = readLine().toDouble
          stocklait(Num3machine) += laitrajouter

          if (laitrajouter < 0) {
            println("quantité non valide, entrer nombre positif.")
            stocklait(Num3machine) -= laitrajouter

          }
        }

      } else {
        if (reaprovisation == 3) {
          sucrerajouter = -1
          while (sucrerajouter <= (-0.0000001)) {
            sucrerajouter = 0
            println("quantité de sucre rajouté : ")
            sucrerajouter = readLine().toInt
            stocksucre(Num3machine) += sucrerajouter


            if (sucrerajouter < 0) {
              println("quantité non valide, entrer nombre positif.")
              stocksucre(Num3machine) -= sucrerajouter

            }

          }


        }
      }
    }




    println("Stocks ajouté:")

    println("poudre a café : " + caferajouter + " g")
    println("Lait : " + laitrajouter + " L")
    println("Sucre : " + sucrerajouter + " g")

    println("Niveaux de stock mis a` jour.")
    println("Retour au menu principal...")

    reaprovisation = 0




  }





  // -------------------------------   SERVICE      CLIENT    -------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------


def serveClient(Num4machine: Int, stockcafeC: Array[Int], stocklaitC: Array[Double], stocksucreC: Array[Int]): Boolean = {

  var x2 = 1
  var boissons = " "
  var laitsupl = " "

  var zucreutiliser = 0
  var laitutiliser = 0.0
  var cafeutiliser = 0

  var prix = 0.0

  var prixsucre = 0.0
  var prixlait = 0.0
  var prixtotal = 0.0

  var y = 0
  var t = 0

  while (!(y == 1 || y == 2 || y == 3)) {
    println("Veuillez sélectionner votre boisson :")
    println("1 pour Expresso - CHF 2.00")
    println("2 pour Cappuchino - CHF 2.50")
    println("3 pour Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
    print("> ")

    y = readLine().toInt

    if (!(y == 1) && !(y == 2) && !(y == 3)) println("Tapé 1, 2 ou 3")

    if (y == 1) {
      prix = 2
      stockcafeC(Num4machine) -= 8
      cafeutiliser += 8
      boissons = "Expresso"
    }
    if (y == 2) {
      prix = 2.5
      boissons = "Capuccino"
      stockcafeC(Num4machine) -= 6
      cafeutiliser += 6
      stocklaitC(Num4machine) -= 0.100
      laitutiliser += 0.100
    }


    if (y == 3) {
      boissons = "Late"

      while (!(t == 1 || t == 2 || t == 3)) {
        println("Taille ? :")
        println("1 pour petit")
        println("2 pour moyen")
        println("3 pour grand")
        print("> ")

        t = readLine().toInt


        if (!(t == 1) && !(t == 2) && !(t == 3)) println("Tapé 1, 2 ou 3")

      }

      if (t == 1) {
        stocklaitC(Num4machine) -= 0.120
        stockcafeC(Num4machine) -= 6
        cafeutiliser += 6
        laitutiliser += 0.120
        prix = 2.7
      } else {
        if (t == 2) {
          stocklaitC(Num4machine) -= 0.150
          stockcafeC(Num4machine) -= 8
          cafeutiliser += 8
          laitutiliser += 0.150

        } else {
          stocklaitC(Num4machine) -= 0.200
          stockcafeC(Num4machine) -= 12
          cafeutiliser += 12
          laitutiliser += 0.200

        }
      }


    }
  }


  var z = 0
  while (!(z == 1 || z == 2 || z == 3 || z == 4)) {
    println("Souhaitez-vous ajouter du sucre ?")
    println("1) Sans sucre")
    println("2) Peu (5g) - CHF 0.10")
    println("3) Moyen (10g) - CHF 0.20")
    println("4) Beaucoup (15g) - CHF 0.30")
    print("> ")


    z = readLine().toInt

    if (!(z == 1) && !(z == 2) && !(z == 3) && !(z == 4)) println("Tapé 1, 2, 3 ou 4")

  }

  if (z == 2) {
    stocksucreC(Num4machine) -= 5
    zucreutiliser += 5
  }
  if (z == 3) {
    stocksucreC(Num4machine) -= 10
    zucreutiliser += 10
  }
  if (z == 4) {
    stocksucreC(Num4machine) -= 15
    zucreutiliser += 15
  }


  //  3 doses maximale par boisson, une dose contiens 50ml de lait)
  var lait = 0
  var dose = 0

  if ((y == 2) || (y == 3)) {


    while (!(lait == 1 || lait == 2)) {
      println("Souhaitez-vous ajouter du lait ?")
      println("(Disponible uniquement pour Cappuccino et Latte)")
      println("1) oui")
      println("2) non")

      print("> ")

      lait = readLine().toInt

      if (!(lait == 1) && !(lait == 2)) println("Tapé 1 ou 2")

      if (lait == 1) {
        while (!(dose == 1 || dose == 2 || dose == 3)) {
          println("nombre de dose : 1, 2 ou 3 : ")
          dose = readLine().toInt
          if (!(dose == 1) && !(dose == 2) && !(dose == 3)) println("Tapé 1, 2 ou 3")
        }
        stocklaitC(Num4machine) = stocklaitC(Num4machine) - (dose * 0.050)
        laitutiliser += (dose * 0.050)

      }
    }
  }
  //--------------------------------------------------------------------------------------------



  if (stockcafeC(Num4machine) < 0) {
    println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionneé.")
    println("Veuillez vérifier les stocks en mode Admin ou choisir une autre Machine")
    stocklaitC(Num4machine) += laitutiliser
    stockcafeC(Num4machine) += cafeutiliser
    stocksucreC(Num4machine) += zucreutiliser
    laitutiliser = 0
    cafeutiliser = 0
    zucreutiliser = 0
    x2 = 0
    return false
  }
  if (stocksucreC(Num4machine) < 0) {
    println("Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionneé.")
    println("Veuillez vérifier les stocks en mode Admin ou choisir une autre Machine")
    stocklaitC(Num4machine) += laitutiliser
    stockcafeC(Num4machine) += cafeutiliser
    stocksucreC(Num4machine) += zucreutiliser
    laitutiliser = 0
    cafeutiliser = 0
    zucreutiliser = 0
    x2 = 0
    return false
  }
  if (stocklaitC(Num4machine) < 0) {
    println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionneé.")
    println("Veuillez vérifier les stocks en mode Admin ou choisir une autre Machine")
    stocklaitC(Num4machine) += laitutiliser
    stockcafeC(Num4machine) += cafeutiliser
    stocksucreC(Num4machine) += zucreutiliser
    laitutiliser = 0
    cafeutiliser = 0
    zucreutiliser = 0
    x2 = 0
    return false
  }



  //------------     PAYER    --------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------------------------




  if (stockcafeC(Num4machine) >= 0 && stocklaitC(Num4machine) >= 0 && stocksucreC(Num4machine) >= 0 && x2 ==1) {


    println("Boisson sélectionné : " + boissons)

    var sucreouinon = " "
    if (z == 1) sucreouinon = "pas de sucre"
    if (z == 2) sucreouinon = "faible (5g)"
    if (z == 3) sucreouinon = "Moyen (10g)"
    if (z == 4) sucreouinon = "Beaucoup (15g)"
    println("Niveau de sucre : " + sucreouinon)

    var laiouinon = " "
    if (lait == 1) laiouinon = "oui"
    if (lait == 2) laiouinon = "non"
    println("Lait en supplément: " + laiouinon)


    if (y == 1) prix = 2
    if (y == 2) prix = 2.5
    if (t == 1) prix = 2.7
    if (t == 2) prix = 3.2
    if (t == 3) prix = 3.7
    if (z == 1) prixsucre = 0
    if (z == 2) prixsucre = 0.1
    if (z == 3) prixsucre = 0.2
    if (z == 4) prixsucre = 0.3
    prixlait = dose * 0.05

    prixtotal = prix + prixlait + prixsucre


    println("Prix Total:  CHF " + prixtotal)


    println("Veuillez payer en utilisant Twint")

    var code = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

    print("Votre code de paiement est : ")
    for (x <- 1 to 5) {
      print((code((math.random() * 61).toInt)))
    }
    println(" ")
    println("(En attente de validation du paiement...)")
    Thread.sleep(3000) // Attend pendant 3000 millisecondes (3 secondes)
    println("Merci ! Votre paiement a été́ accepte ́.")

    println("Préparation de votre boisson...")
    println("[...]")
    Thread.sleep(5000) // Attend pendant 5000 millisecondes (5 secondes)
    println("Votre " + boissons + " est prêt ! Bonne dégustation !")

    return true
  }

  return false
}














  // --------------------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------








  def main(args: Array[String]): Unit = {
    import io.StdIn._
import scala.math._

    // x == qui
    // y == boisson
    // z == sucre
    // t == latte taille

    var x = 0

    var sslait2= Array( 0.500,0.500,0.500,0.500,0.500)
    var sssucre2= Array(30,30,30,30,30)
    var sscafe2= Array(50,50,50,50,50)

    val nbmachine = 5
    val machid = Array.tabulate(nbmachine)(i => i)

    var machPIN = Array[Int](434343,434343,434343,434343,434343)  // code pin ICI !!!!!!!!!!!!!!!!!!!!!!!!!
    var id = 10





    // -------------------------   DEBUT     PROGRAMME    -------------------------------------------------------------------------------------------
    // -------------------------------------------------------------------------------------------------------------------
    // --------------------------------------------------------------------------------------------------------------------
    // -------------------------------------------------------------------------------------------------------------------

    do {

      x = 0

      var boissons = " "
      var laitsupl = " "


      while (!(x == 1 || x == 2 || x == 3)) {

        println("Veuillez sélectionner votre mode :")
        println("1 pour Client")
        println("2 pour Admin")
        println("3 pour Quitter")
        print("> ")

        x = readLine().toInt

        if (!(x == 1) && !(x == 2) && !(x == 3)) println("Tapé 1, 2 ou 3")

      }




      //



      if (x == 1) {

        while (!(id== 0 ||id == 1 || id == 2 || id == 3|| id ==4)) {
          println("selectionné L'id de la machine > ")
          id = readLine().toInt
          println(id)
          if (!(id== 0 ||id == 1 || id == 2 || id == 3|| id ==4)) println("Tapé 0, 1, 2, 3 ou 4")
        }

        if( serveClient(machid(id), sscafe2,sslait2,sssucre2) ==true){
          println("transaction REUSSITE")
        }else{
          println("transaction ECHOUEE")
        }


        id = 10
        x = 0


      } else {


        //--------------      ADMIN      ------------------------------------------------------------------------------
        //--------------------------------------------------------------------------------------------------------------------------------------
        //--------------------------------------------------------------------------------------------------------------------------------------




        if (x == 2) {
          println("Mode ADMIN")


          while (!(id== 0 ||id == 1 || id == 2 || id == 3|| id ==4)) {
            println("5 machine a disponition")
            println("machine zone 0")
            println("machine zone 1")
            println("machine zone 2")
            println("machine zone 3")
            println("machine zone 4")
            println("selectionné L'id de la machine (choisir entre 0 a 4)  > ")
            id = readLine().toInt
            if (!(id== 0 ||id == 1 || id == 2 || id == 3|| id ==4)) println("Tapé 0, 1, 2, 3 ou 4")
          }

          var machPINfinal = false
          machPINfinal = validatePIN(machid(id), machPIN)
          if( machPINfinal== false){
           x = 3
          }else {


            var changerCODE = 0

            while (!(changerCODE== 1 || changerCODE== 2)) {
              println("changer de CODE (nécéssite réinitalitation) ?  1 = OUI, 2 = NON")
              changerCODE = readLine().toInt

              if (!(changerCODE== 1 ||changerCODE == 2)) println("Tapé 1 ou 2")

              if(changerCODE== 1){
                println("entrer nouveau code a 6 chiffre pour la machine "+machPIN(id)+" > ")
                UpdatePIN(machid(id), machPIN)
                x=0
              }else{if(changerCODE==2) {
                restockmachine(machid(id), sscafe2,sslait2,sssucre2)
              }
              }

            }

            id = 10
          }

        }
      }

    }
    while( !(x==3))

    println("fin du programme")






















  }
}