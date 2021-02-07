
<?php
   // On vérifie si la fonction ini_set() a été désactivée...
   $desactive = ini_get('disable_functions');
   if (preg_match("/ini_set/i", "$desactive") == 0) {
      // Si elle n'est pas désactivée, on définit ini_set de manière à n'afficher que les erreurs...
      ini_set("error_reporting" , "E_ALL & ~E_NOTICE");
   }
   // Vérifier que le formulaire a été envoyé...
   if (isset($_POST['envoi'])) {
      //On commence une session pour enregistrer les variables du formulaire...
      session_start();
      $_SESSION['champ1'] = $_POST['champ1'];
      $_SESSION['champ2'] = $_POST['champ2'];
      $_SESSION['zone_email1'] = $_POST['zone_email1'];
      //Enregistrement des zones de texte...
      $_SESSION['zone_texte1'] = $_POST['zone_texte1'];
      // Définir l\'icone apparaissant en cas d\'erreur...
      // Définir sur 0 pour afficher un petit x de couleur rouge.
      // Définir sur 1 pour afficher l\'image d\'une croix rouge telle que celle utilisée dans l\'assistant
      // Si vous utilisez l\'option 1, l\'image de la croix rouge \'icone.gif\' doit se trouver dans le répertoire \'images\',
      // ce dernier devant se trouver au même niveau que votre formulaire...
      $flag_icone = 0;
      // On vérifie si $flag_icone est défini sur 0 ou 1...
      if ($flag_icone == 0) {
         $icone = "<b><font size=\"3\" face=\"Arial, Verdana, Helvetica, sans-serif\" color=\"#CC0000\">x</font></b>";
      } else {
         $icone = "<img src=\"images/icone.gif\"";
      }
      // Définir l'indicateur d'erreur sur zéro...
      $flag_erreur = 0;
      // N'envoyer le formulaire que s'il n'y a pas d'erreurs...
      if ($flag_erreur == 0) {   
         // Addresse de réception du formulaire
         $ip = $_SERVER['REMOTE_ADDR'];
         $dt = date("l dS \of F Y h:i:s A");
         $data = "<font face=\"Verdana\" size=\"2\" color=\"#003366\"> IP ( "  . $ip . " " . $dt . " )</font><br>\n";
         $email_dest = "safwan.aljbaae@gmail.com,safwanaljbaae@yahoo.com," . $_SESSION['zone_email1'];
         $sujet = $_SESSION['champ2'];
         $entetes ="MIME-Version: 1.0 \n";
         $entetes .="From:" . $_SESSION['zone_email1'] . "\n";
         $entetes .="Return-Path: ...<safwan.aljbaae@gmail.com,safwanaljbaae@yahoo.com>\n";
         $entetes .="Reply-To:" . $_SESSION['zone_email1'] . "\n";
         $entetes .="Content-Type: text/html; charset=iso-8859-1 \n";
         $partie_entete = "<html>\n<head>\n<title>Formulaire</title>\n<meta http-equiv=Content-Type content=text/html; charset=iso-8859-1>\n</head>\n<body bgcolor=#FFFFFF>\n";
         //Partie HTML de l'e-mail...
         $partie_champs_texte .= "<font face=\"Verdana\" size=\"2\" color=\"#003366\"> name = " . $_SESSION['champ1'] . "</font><br>\n";
         $partie_champs_texte .= "<font face=\"Verdana\" size=\"2\" color=\"#003366\">Subject of the message = " . $_SESSION['champ2'] . "</font><br>\n";
         $partie_zone_email .= "<font face=\"Verdana\" size=\"2\" color=\"#003366\">E-mail = " . $_SESSION['zone_email1'] . "</font><br>\n";
         $partie_zone_texte .= "<font face=\"Verdana\" size=\"2\" color=\"#003366\">The message = " . $_SESSION['zone_texte1'] . "</font><br>\n ";
         // Fin du message HTML
         $fin = "</body></html>\n\n";
         $sortie = $partie_entete . $partie_champs_texte . $partie_zone_email . $partie_listes . $partie_boutons . $data . $partie_cases . $partie_zone_texte . $fin;
         // Send the e-mail
         if (@!mail($email_dest,$sujet,$sortie,$entetes)) {
            echo("Impossible to sent");
            exit();
         } else {
                // Rediriger vers la page de remerciement
//             echo('<meta http-equiv="refresh" content="0; URL="Thank_you_CPM.html">');   
                exit();
            } // Fin else
      } // Fin du if ($flag_erreur == 0) {
   } // Fin de if POST
?>
