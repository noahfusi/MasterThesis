import scala.io.StdIn

object machineCafé {
  def main (args: Array[String]): Unit = {
    var mode: Boolean = true

    var stockCafé: Int= 50
    var stockSucre: Int= 30
    var stockLait: Double= 0.5

    val caférequisex: Int= 8
    val laitrequisex: Double= 0.0

    val caférequiscap: Int= 6
    val laitrequiscap: Double= 0.1

    val caférequislatp: Int= 6
    val laitrequislatp: Double= 0.12
    val caférequislatm: Int= 8
    val laitrequislatm: Double = 0.15
    val caférequislatg: Int= 12
    val laitrequislatg: Double= 0.2

    val laitrequissuppx1: Double = 0.05
    val laitrequissuppx2: Double= 0.1
    val laitrequissuppx3: Double= 0.15

    val sucrerequispeu: Int= 5
    val sucrerequismoyen:Int= 10
    val sucrerequisbcp: Int= 15

    def suppsucrepeu (supplément: String, sucrerequispeu:Int ): Unit= {
      if (stockSucre >= sucrerequispeu) {
        println("preparation de votre boisson...")
        stockSucre= stockSucre - sucrerequispeu
        Thread.sleep(2000)
      } else{
        println("stock insuffisant, pas de supplément sucre.")
      }
    }
    def suppsucremoyen (supplément: String, sucrerequismoyen:Int ): Unit= {
      if (stockSucre >= sucrerequismoyen) {
        println("preparation de votre boisson...")
        stockSucre= stockSucre - sucrerequismoyen
        Thread.sleep(2000)
      } else{
        println("stock insuffisant, autre supplément sucre.")
      }
    }
    def suppsucrebcp (supplément: String, sucrerequisbcp:Int ): Unit= {
      if (stockSucre >= sucrerequisbcp) {
        println("preparation de votre boisson...")
        stockSucre= stockSucre - sucrerequisbcp
        Thread.sleep(2000)
      } else{
        println("stock insuffisant, autre supplément sucre.")
      }
    }



    def prepalaitsuppx1(supplément: String, laitrequissuppx1: Double): Unit= {
      if (stockLait >= laitrequissuppx1) {
        println("preparation de votre boisson...")
        stockLait= stockLait - laitrequissuppx1
        Thread.sleep(2000)
        println(" autres suppléments?")
        Thread.sleep(2000)
      } else{
        println("stock insuffisant, pas de supplément lait.")
      }
    }

    def prepalaitsuppx2(supplément: String, laitrequissuppx2: Double): Unit= {
      if (stockLait >= laitrequissuppx2) {
        println("preparation de votre boisson...")
        stockLait= stockLait - laitrequissuppx2
        Thread.sleep(2000)
        println(" autres suppléments?")
        Thread.sleep(2000)
      } else{
        println("stock insuffisant, autre supplément lait.")
      }
    }

    def prepalaitsuppx3(supplément: String, laitrequissuppx3: Double): Unit= {
      if (stockLait >= laitrequissuppx3) {
        println("preparation de votre boisson...")
        stockLait= stockLait - laitrequissuppx3
        Thread.sleep(2000)
        println(" autres suppléments?")
        Thread.sleep(2000)
      } else{
        println("stock insuffisant, autre supplément lait.")
      }
    }


    def prepareexpresso(boisson: String, caferéquisex: Int, laitrequisex: Double): Unit= {
      if (stockCafé >= caférequisex && stockLait >= laitrequisex) {
        println("preparation de votre boisson...")
        stockCafé= stockCafé - caferéquisex
        stockLait= stockLait - laitrequisex
        Thread.sleep(2000)
        println("suppléments?")
        Thread.sleep(2000)
      } else{
        println("stock insuffisant, choisir une autre boisson.")

      }
    }

    def preparecap(boisson: String, caférequiscap:Int, laitrequiscap: Double): Unit= {
      if (stockCafé >= caférequiscap && stockLait>= laitrequiscap) {
        println("preparation de votre boisson")
        stockCafé= stockCafé - caférequiscap
        stockLait= stockLait - laitrequiscap
        Thread.sleep(2000)
      } else {
        println("stock insuffisant, choisir une autre boisson.")
      }
    }

    def preparelatp( boisson: String, caférequislatp:Int, Laitrequislatp: Double): Unit= {
      if (stockCafé >= caférequislatp && stockLait>= laitrequislatp) {
        println("preparation de votre boisson")
        stockCafé= stockCafé - caférequislatp
        stockLait= stockLait - laitrequislatp
        Thread.sleep(2000)
      } else {
        println("stock insuffisant, choisir une autre boisson.")
      }
    }

    def preparelatm( boisson: String, caférequislatm:Int, Laitrequislatm: Double): Unit= {
      if (stockCafé >= caférequislatm && stockLait>= laitrequislatm) {
        println("preparation de votre boisson")
        stockCafé= stockCafé - caférequislatm
        stockLait= stockLait - laitrequislatm
        Thread.sleep(2000)
      } else {
        println("stock insuffisant, choisir une autre boisson.")
      }
    }

    def preparelatg( boisson: String, caférequislatg:Int, Laitrequislatg: Double): Unit= {
      if (stockCafé >= caférequislatg && stockLait>= laitrequislatg) {
        println("preparation de votre boisson")
        stockCafé= stockCafé - caférequislatg
        stockLait= stockLait - laitrequislatg
        Thread.sleep(2000)
      } else {
        println("stock insuffisant, choisir une autre boisson.")
      }
    }


    while (mode) {
      println("Nospresso café")
      println("veuillez selectionner votre mode")
      println("1) client")
      println("2) admin ")
      println("3) quitter")
      println(">")
      // choix du mode
      val choix= StdIn.readLine()

      if (choix=="1") {
        Thread.sleep(2000)
        println("mode client selectionné")
        var modeclient : Boolean= true
        while (modeclient){
          println("veuillez choisir une boisson")
          println("1.1) Expresso - CHF 2.00")
          println("1.2) cppucino- CHF 2.50")
          println("1.3) latte- CHF 2.70 (petit); CHF 3.20 (moyen); CHF 3.70 (grand)")
          println ("1.4) annuler")
          println ( ">")

          val choixdeboisson = StdIn.readLine()
          if (choixdeboisson == "1.1") {
            Thread.sleep(2000)
            println("vous avez choisi un expresso")
            prepareexpresso("expresso",caférequisex,laitrequisex )
            println("souhaites-vous rajouter du sucre?")
            var personalisation: Boolean= true
            while (personalisation){
              println("1.1.1) sans sucre")
              println("1.1.2) peu- 5g- CHF 0.10")
              println("1.1.3) moyen- 10g- CHF 0.20")
              println("1.1.4) beacoup- 15g- CHF 0.30")
              print(">")
              val personalisationex= StdIn.readLine()

              if (personalisationex=="1.1.1"){
                println("sans sucre")
              }else if (personalisationex=="1.1.2"){
                println("peu")
                suppsucrepeu("peu",sucrerequispeu)
              } else if (personalisationex=="1.1.3"){
                println("moyen")
                suppsucremoyen("moyen",sucrerequismoyen)
              }else if (personalisationex=="1.1.4"){
                println("beacoup")
                suppsucrebcp("beacoup",sucrerequisbcp)
              }
              Thread.sleep(2000)
              println("souhaitez-vous ajouter du lait supplémentaire?")
              var laitsupp: Boolean= true
              while (laitsupp){
                println("#) oui")
                println("*) non")
                println(">")
                val laitensupp= StdIn.readLine()
                if (laitensupp=="#"){
                  println("combien de dose?")
                  var dosette: Boolean= true
                  while (dosette){
                    println("0")
                    println("1")
                    println("2")
                    println("3")
                    println(">")
                    val dose= StdIn.readLine()
                    if (dose=="0"){
                      println("patienter...")
                    } else if (dose=="1"){
                      prepalaitsuppx1("1 dose",laitrequissuppx1)
                      println("patienter...")
                    } else if (dose=="2"){
                      prepalaitsuppx2("2 doses",laitrequissuppx2)
                      println("patienter...")
                    } else if (dose=="3"){
                      prepalaitsuppx3("3 doses",laitrequissuppx3)
                      println("patienter...")

                    }
                  }
                }
              }
            }

          } else if (choixdeboisson== "1.2"){
            println("vous avez choisi cappucino")
            preparecap("cappucino",caférequiscap,laitrequiscap)
            println("souhaites-vous rajouter du sucre?")
            var personalisation: Boolean= true
            while (personalisation){
              println("1.2.1) sans sucre")
              println("1.2.2) peu- 5g- CHF 0.10")
              println("1.2.3) moyen- 10g- CHF 0.20")
              println("1.2.4) beacoup- 15g- CHF 0.30")
              println(">")
              val personalisationex= StdIn.readLine()

              if (personalisationex=="1.2.1"){
                println("sans sucre")
              }else if (personalisationex=="1.2.2"){
                println("peu")
                suppsucrepeu("peu",sucrerequispeu)
              } else if (personalisationex=="1.2.3"){
                println("moyen")
                suppsucremoyen("moyen",sucrerequismoyen)
              }else if (personalisationex=="1.2.4"){
                println("beacoup")
                suppsucrebcp("beacoup", sucrerequisbcp)
              }
              println("souhaitez-vous ajouter du lait supplémentaire?")
              var laitsupp: Boolean= true
              while (laitsupp){
                println("#) oui")
                println("*) non")
                println(">")
                val laitensupp= StdIn.readLine()
                if (laitensupp=="#"){
                  println("combien de dose?")
                  var dosette: Boolean= true
                  while (dosette){
                    println("0")
                    println("1")
                    println("2")
                    println("3")
                    println(">")
                    val dose= StdIn.readLine()
                    if (dose=="0"){
                      println("patienter...")
                    } else if (dose=="1"){
                      prepalaitsuppx1("1 doses",laitrequissuppx1)
                      println("patienter...")
                    } else if (dose=="2"){
                      prepalaitsuppx2("2 doses",laitrequissuppx2)
                      println("patienter...")
                    } else if (dose=="3"){
                      prepalaitsuppx3("3 doses",laitrequissuppx3)
                      println("patienter...")
                    }
                  }
                }
              }
            }


          } else if (choixdeboisson=="1.3") {
            println("vous avez choisi un latte")
            var latte: Boolean= true
            while(latte){
              println("1.3.1) petit- CHF 2.70")
              println("1.3.2) moyen- CHF 3.20")
              println("1.3.3) grand- CHF 3.70")
              println("1.3.4) retour")
              println(">")

              val taillelatte = StdIn.readLine()

              if (taillelatte=="1.3.1"){
                println("petit")
                preparelatp("latte petit",caférequislatp, laitrequislatp)
                println("souhaites-vous rajouter du sucre?")
                var personalisation: Boolean= true
                while (personalisation) {
                  println("1.3.5) sans sucre")
                  println("1.3.6) peu- 5g- CHF 0.10")
                  println("1.3.7) moyen- 10g- CHF 0.20")
                  println("1.3.8) beacoup- 15g- CHF 0.30")
                  println(">")

                  val personalisationex= StdIn.readLine()

                  if (personalisationex=="1.3.5"){
                    println("sans sucre")
                  } else if (personalisationex=="1.3.6"){
                    println("peu")
                    suppsucrepeu("peu",sucrerequispeu)
                  } else if (personalisationex=="1.3.7"){
                    println("moyen")
                    suppsucremoyen("moyen", sucrerequismoyen )
                  } else if (personalisationex=="1.3.8"){
                    println("beaucoup")
                    suppsucrebcp("beaucoup",sucrerequisbcp)
                  }
                  println("souhaitez-vous ajouter du lait supplémentaire?")
                  var laitsupp: Boolean= true
                  while (laitsupp) {
                    println("#) oui")
                    println("*) non")
                    val laitensupp= StdIn.readLine()
                    if (laitensupp=="#"){
                      println("combien de dose?")
                      var dosette: Boolean= true
                      while (dosette){
                        println("0")
                        println("1")
                        println("2")
                        println("3")
                        val dose= StdIn.readLine()
                        if (dose=="0"){
                          println("patienter... ")
                        } else if (dose=="1"){
                          prepalaitsuppx1("1 doses",laitrequissuppx1)
                          println("patienter... ")
                        } else if (dose=="2"){
                          prepalaitsuppx2("2 doses",laitrequissuppx2)
                          println("patienter...")
                        } else if (dose=="3"){
                          prepalaitsuppx3("3 doses",laitrequissuppx3)
                          println("patienter... ")
                        }
                      }
                    }

                  }


                }
              } else if (taillelatte=="1.3.2"){
                println("moyen")
                preparelatm("latte moyen", caférequislatm, laitrequislatm)
                println("souhaites-vous rajouter du sucre?")
                var personalisation: Boolean= true
                while (personalisation){
                  println("1.3.5) sans sucre")
                  println("1.3.6) peu- 5g- CHF 0.10")
                  println("1.3.7) moyen- 10g- CHF 0.20")
                  println("1.3.8) beacoup- 15g- CHF 0.30")
                  println(">")

                  val personalisationex= StdIn.readLine()

                  if (personalisationex=="1.3.5"){
                    println("sans sucre")
                  } else if (personalisationex=="1.3.6"){
                    println("peu")
                    suppsucrepeu("peu",sucrerequispeu)
                  } else if (personalisationex=="1.3.7"){
                    println("moyen")
                    suppsucremoyen("moyen", sucrerequismoyen )
                  } else if (personalisationex=="1.3.8"){
                    println("beaucoup")
                    suppsucrebcp("beaucoup",sucrerequisbcp)
                  }
                  println("souhaitez-vous ajouter du lait supplémentaire?")
                  var laitsupp: Boolean= true
                  while (laitsupp) {
                    println("#) oui")
                    println("*) non")
                    val laitensupp= StdIn.readLine()
                    if (laitensupp=="#"){
                      println("combien de dose?")
                      var dosette: Boolean= true
                      while (dosette){
                        println("0")
                        println("1")
                        println("2")
                        println("3")
                        val dose= StdIn.readLine()
                        if (dose=="0"){
                          println("patienter...")
                        } else if (dose=="1"){
                          prepalaitsuppx1("1 doses",laitrequissuppx1)
                          println("patienter... ")
                        } else if (dose=="2"){
                          prepalaitsuppx2("2 doses",laitrequissuppx2)
                          println("patienter...")
                        } else if (dose=="3"){
                          prepalaitsuppx3("3 doses",laitrequissuppx3)
                          println("patienter...")
                        }
                      }
                    }

                  }


                }
              } else if (taillelatte=="1.3.3") {
                println("grand")
                preparelatg("latte grand", caférequislatg, laitrequislatg)
                println("souhaites-vous rajouter du sucre?")
                var personalisation: Boolean= true
                while (personalisation){
                  println("1.3.5) sans sucre")
                  println("1.3.6) peu- 5g- CHF 0.10")
                  println("1.3.7) moyen- 10g- CHF 0.20")
                  println("1.3.8) beacoup- 15g- CHF 0.30")
                  println(">")

                  val personalisationex= StdIn.readLine()

                  if (personalisationex=="1.3.5"){
                    println("sans sucre")
                  } else if (personalisationex=="1.3.6"){
                    println("peu")
                    suppsucrepeu("peu",sucrerequispeu)
                  } else if (personalisationex=="1.3.7"){
                    println("moyen")
                    suppsucremoyen("moyen", sucrerequismoyen )
                  } else if (personalisationex=="1.3.8"){
                    println("beaucoup")
                    suppsucrebcp("beaucoup",sucrerequisbcp)
                  }
                  println("souhaitez-vous ajouter du lait supplémentaire?")
                  var laitsupp: Boolean= true
                  while (laitsupp) {
                    println("#) oui")
                    println("*) non")
                    val laitensupp= StdIn.readLine()
                    if (laitensupp=="#"){
                      println("combien de dose?")
                      var dosette: Boolean= true
                      while (dosette){
                        println("0")
                        println("1")
                        println("2")
                        println("3")
                        val dose= StdIn.readLine()
                        if (dose=="0"){
                          println("patienter... ")
                        } else if (dose=="1"){
                          prepalaitsuppx1("1 doses",laitrequissuppx1)
                          println("patienter...")
                        } else if (dose=="2"){
                          prepalaitsuppx2("2 doses",laitrequissuppx2)
                          println("patienter...")
                        } else if (dose=="3"){
                          prepalaitsuppx3("3 doses",laitrequissuppx3)
                          println("patienter... ")
                        }
                      }
                    }

                  }


                }
                 if (personalisation) {
                  def main  (args: Array[String]): Unit = {
                    var laitsupp = true // Exemple pour contrôle de boucle while

                    while (laitsupp) {
                      // Votre logique principale ici
                      println("Préparation en cours...")

                      // Exemple de contrôle pour sortir de la boucle
                      laitsupp = false // Remplacez par votre condition réelle
                    }

                    // Déclaration de l'objet en dehors de la boucle
                    object CalculPrixBoisson {
                      def calculerPrixFinal(
                                             prixBase: Double,
                                             niveauSucre: Int,
                                             dosesSupplementaires: Int
                                           ): Double = {
                        val prixSucre = if (niveauSucre == 0) 0.0
                        else if (niveauSucre == 1) 0.10
                        else if (niveauSucre == 2) 0.20
                        else if (niveauSucre == 3) 0.30
                        else 0.0

                        val prixDoses = dosesSupplementaires * 0.50
                        prixBase + prixSucre + prixDoses
                      }
                    }
                  }
                }

              }else if (taillelatte=="1.3.4"){
                println("retour au menu boisson")
                latte= false
              }
            }
          } else if (choix=="1.4") {
            println("annulation de commande")
            modeclient= false
          }
        }


      }else if (choix=="2"){
        println("mode admin selectionné")
        var modeadmin: Boolean= true
        while (modeadmin){
          Thread.sleep(2000)
          println("saisir le code")
          println(">")
          val code=StdIn.readLine()
          if (code=="434343") {
            println("accès autorisé...")
            Thread.sleep(2000)
            var stockCafé: Int= 50
            var stockSucre: Int= 30
            var stockLait: Double= 0.5

            var reapprovisionement: Boolean = true
            while (reapprovisionement){
              println("...gestion des stocks")
              Thread.sleep(2000)
              println("stocks actuels : ")
              println(s"2.1)poudre de café: ${stockCafé}g")
              println(s"2.2)sucre: ${stockSucre}g")
              println(s"2.3)lait: ${stockLait}L")
              println("reapprovisionement:")
              println("2.1)poudre de café")
              println("2.2)sucre")
              println("2.3)lait")
              println("2.4) quitter mode admin")
              println(">")
              val choixstock= StdIn.readLine()
              if (choixstock=="2.1"){
                println("ajouter (en grammes): ")
                val ajoutCafé = StdIn.readLine().toInt
                stockCafé= stockCafé + ajoutCafé
                println(s"stock mis à jour: ${stockCafé}g")
                println("retour au menu principal...")
              } else if (choixstock=="2.2"){
                println("ajouter (en grammes): ")
                val ajoutSucre: Int = StdIn.readLine().toInt
                stockSucre = stockSucre + ajoutSucre
                println(s"stock mis à jour: ${stockSucre}g")
                println("retour au menu principal...")
              } else if (choixstock=="2.3") {
                println("ajouter (en litres): ")
                val ajoutlait= StdIn.readLine().toDouble
                stockLait= stockLait + ajoutlait
                println(s"stock mis à jour: ${stockLait}L")
                println("retour au menu principal...")
              } else if (choixstock=="2.4") {
                reapprovisionement= false
                modeadmin= false
              }
            }


          } else {
            Thread.sleep((2000))
            println("code incorrecte")
            modeadmin=false
          }
        }
      } else if (choix=="3"){println("annulation de procedure")
        mode = false
      }
    }
  }
}


