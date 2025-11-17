object Main {
  var stockCafe = 50.0
  var stockSucre = 30.0
  var stockLait = 0.5

  def main(args: Array[String]): Unit = {
    def afficherTexteCaractereParCaractere(texte: String, delai: Int = 5): Unit = {
      for (caractere <- texte) {
        print(caractere)
        Thread.sleep(delai)
      }
      println()
    }

    def afficherStocks(): Unit = {
      println(s"\nStocks :")
      println(f"- Poudre de café : $stockCafe%.2f g")
      println(f"- Lait : ${stockLait}" + "L")
      println(f"- Sucre : $stockSucre%.2f g")
    }

    def ajouterStock(): Unit = {
      println("\nRéapprovisionnement des stocks :")
      println("Ajout :")
      print("Quantité de café à ajouter (g) : ")
      stockCafe += readLine().toDouble
      print("Quantité de sucre à ajouter (g) : ")
      stockSucre += readLine().toDouble
      print("Quantité de lait à ajouter (L) : ")
      stockLait += readLine().toDouble
      afficherStocks()
      println("Niveau de stock mis à jour.")
      println("Retour au menu principal...")
      demarrerProgramme() // Revenir au menu principal
    }

    def verifierStock(cafe: Double, lait: Double, sucre: Double): Boolean = {
      if (stockCafe < cafe) {
        println(s"Erreur : Quantité de café insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez chosiir une autre boisson ou vérifier les stocks en mode Admin.")
        return false
      }
      if (stockLait < lait / 1000.0) {
        println(s"Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez chosiir une taille plus petite ou essayer une autre boisson.")
        return false
      }
      if (stockSucre < sucre) {
        println(s"Erreur : Quantité de sucre insuffisante pour préparer la boisson sélectionnée.")
        println("Veuillez chosiir une autre boisson ou vérifier les stocks en mode Admin.")
        return false
      }
      true
    }


    def deduireStock(cafe: Double, lait: Double, sucre: Double): Unit = {
      stockCafe -= cafe
      stockLait -= lait / 1000.0
      stockSucre -= sucre
      println("Stocks mis à jour après la transaction.")
      afficherStocks()
    }

    def demarrerProgramme(): Unit = {
      println("\nNospresso Café")
      println("Veuillez sélectionner votre mode")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")

      var EntreeUtilisateur = 0
      do {
        EntreeUtilisateur = readLine("> ").toInt
        if (EntreeUtilisateur < 1 || EntreeUtilisateur > 3) {
          println("Veuillez entrer une valeur valide entre 1 et 3.")
        }
      } while (EntreeUtilisateur < 1 || EntreeUtilisateur > 3)

      if (EntreeUtilisateur == 2) {
        println("Entrez le code PIN :")
        val motDePasse = readLine("> ")

        if (motDePasse == "434343") {
          println("Accès autorisé.")
          afficherStocks()
          println("Souhaitez-vous ajouter des stocks ?\n1) oui\n2) non")

          var choix = ""
          do {
            choix = readLine("> ").trim
            if (choix == "1") {
              ajouterStock()
              return
            } else if (choix == "2") {
              println("Retour au menu principal...")
              demarrerProgramme()
              return
            } else {
              println("Veuillez entrer une valeur valide entre 1 et 2.")
            }
          } while (choix != "1" && choix != "2")
        } else {
          println("Mot de passe incorrect. Retour au menu principal.")
          demarrerProgramme()
          return
        }
      }

      if (EntreeUtilisateur == 1) {
        var ChoixBoisson = ""
        var PrixBase = 0.0
        var NomBoisson = ""
        var cafeRequis = 0.0
        var laitRequis = 0.0

        do {
          println("Veuillez sélectionner votre boisson : ")
          println("1) Expresso - CHF 2.00")
          println("2) Cappuccino - CHF 2.50")
          println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
          ChoixBoisson = readLine("> ").toLowerCase
          ChoixBoisson match {
            case "1" =>
              PrixBase = 2.00
              NomBoisson = "Expresso"
              cafeRequis = 8.0
              laitRequis = 0.0
            case "2" =>
              PrixBase = 2.50
              NomBoisson = "Cappuccino"
              cafeRequis = 6.0
              laitRequis = 100.0
            case "3" =>
              println("Veuillez sélectionner la taille : ")
              println("1) Petit - CHF 2.70")
              println("2) Moyen - CHF 3.20")
              println("3) Grand - CHF 3.70")
              val Taille = readLine("> ")
              Taille match {
                case "1" =>
                  PrixBase = 2.70
                  NomBoisson = "Latte Petit"
                  cafeRequis = 6.0
                  laitRequis = 120.0
                case "2" =>
                  PrixBase = 3.20
                  NomBoisson = "Latte Moyen"
                  cafeRequis = 8.0
                  laitRequis = 150.0
                case "3" =>
                  PrixBase = 3.70
                  NomBoisson = "Latte Grand"
                  cafeRequis = 12.0
                  laitRequis = 200.0
                case _ => println("Veuillez sélectionner une taille valide.")
              }
            case _ => println("Veuillez saisir une boisson possible.")
          }
        } while (ChoixBoisson != "1" && ChoixBoisson != "2" && ChoixBoisson != "3")

        var PrixLait = 0.0
        var LaitSupp = "Non"
        var choixLait = ""
        var sucreRequis = 0.0
        var PrixSucre = 0.0

        if (ChoixBoisson == "2" || ChoixBoisson == "3") {
          do {
            println("Souhaitez-vous ajouter du lait en supplément ? (Disponible uniquement pour Cappuccino et Latte)")
            println("1) Oui")
            println("2) Non")
            choixLait = readLine("> ").trim

            if (choixLait == "1") {
              var dosesLait = 0
              do {
                dosesLait = readLine("Combien de doses ? : ").toInt
                if (dosesLait < 1) {
                  println("Veuillez saisir un nombre de doses valide (au moins 1).")
                } else {
                  // Prix pour les doses normales (jusqu'à 3)
                  val dosesGratuites = Math.min(dosesLait, 3)
                  val dosesSupplementaires = Math.max(0, dosesLait - 3)

                  PrixLait = dosesGratuites * 0.20 + dosesSupplementaires * 0.05
                  LaitSupp = s"$dosesLait dose(s)"
                  laitRequis += dosesLait * 50.0

                  if (dosesSupplementaires > 0) {
                  }
                }
              } while (dosesLait < 1)
            } else if (choixLait != "2") {
              println("Réponse invalide, veuillez saisir '1' pour Oui ou '2' pour Non.")
            }
          } while (choixLait != "1" && choixLait != "2")
        }

        var prixSucre = 0.0
        var sucreChoix = ""
        do {
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          sucreChoix = readLine("> ")

          sucreChoix match {
            case "1" => sucreRequis = 0.0
            case "2" => {
              sucreRequis = 5.0
              prixSucre = 0.10
            }
            case "3" => {
              sucreRequis = 10.0
              prixSucre = 0.20
            }
            case "4" => {
              sucreRequis = 15.0
              prixSucre = 0.30
              if (NomBoisson == "Expresso" && stockCafe <0.08) {
                println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée.")
                println("Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
                demarrerProgramme()
                return
              }
            }
            case _ => println("Option de sucre invalide.")
          }
        } while (!List("1", "2", "3", "4").contains(sucreChoix))

        if (!verifierStock(cafeRequis, laitRequis, sucreRequis)) {
          demarrerProgramme()
          return
        }

        val PrixTotal = BigDecimal(PrixBase + PrixLait + prixSucre).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
        println(f"Prix total : CHF $PrixBase%.2f + CHF $PrixLait%.2f + CHF $prixSucre%.2f = CHF $PrixTotal%.2f")
        println("")

        def genererCodePaiement(): String = {
          val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
          val random = new scala.util.Random
          var codePaiement = ""

          for (_ <- 1 to 5) {
            val indexAleatoire = random.nextInt(chars.length)
            codePaiement += chars(indexAleatoire)
          }

          codePaiement
        }

        val codePaiement = genererCodePaiement()
        println(s"Veuillez payer en utilisant Twint.\nVotre code de paiement est : $codePaiement \n(En attente de paiement...)")
        Thread.sleep(3000)
        println("\nPaiement confirmé..")
        println("Préparation de votre boisson...")


        println("Votre " + (ChoixBoisson match {
          case "1" => "Expresso"
          case "2" => "Cappuccino"
          case "3" => "Latte"
          case _ => "Boisson"
        }) + " est prêt ! Bonne dégustation !\n")
        deduireStock(cafeRequis, laitRequis, sucreRequis)
        demarrerProgramme() // Revenir au menu principal après la préparation de boisson
      }
    }

    demarrerProgramme()
  }
}
