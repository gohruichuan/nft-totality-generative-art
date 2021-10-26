# library(httr)
# set_config(config(ssl_verifypeer = 0L))
library(generativeart) # devtools::install_github("cutterkom/generativeart")
library(pracma)
library(jsonlite) # install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')
# library("rjson")
library(ggfx)
library(gganimate)
# library(dplyr) # install.packages("dplyr") first
# library(jasmines) # devtools::install_github("djnavarro/jasmines")
# set the paths
IMG_DIR <- "img/"
IMG_SUBDIR <- "everything/"
IMG_SUBDIR2 <- "handpicked/"
IMG_PATH <- paste0(IMG_DIR, IMG_SUBDIR)

LOGFILE_DIR <- "logfile/"
LOGFILE <- "logfile.csv"
LOGFILE_PATH <- paste0(LOGFILE_DIR, LOGFILE)

nr_of_img <- 1919
JSON_metadata <- list()
inc <- function(x) {
    eval.parent(substitute(x <- x + 1))
}
# create the directory structure
generativeart::setup_directories(IMG_DIR, IMG_SUBDIR, IMG_SUBDIR2, LOGFILE_DIR)

# include a specific formula, for example:

getFomula <- function() {
    rng <- sample(1:100, size = 1)

    if(rng <= 3){
        print("trait Stellar Collision")
        list(
            trait = "Stellar Collision",
            x_polarity = runif(1, -1, 1),
            y_polarity = runif(1, -1, 1),
            x_iRNG = sample(2:5, size = 1),
            y_iRNG = sample(3:5, size = 1),
            x_trigonometricFunction = "Cosecant",
            y_trigonometricFunction = "Cosecant",
            x = quote(x_polarity * x_i^x_iRNG - cot(y_i^sample(2:2, size = 1))),
            y = quote(y_polarity * y_i^y_iRNG - cot(x_i^sample(2:2, size = 1)) * y_i^sample(1:20, size = 1))
        )
    }
    else if (rng >= 4 && rng <= 5){
        print("trait sirius2")
        list(
            trait = "Sirius",
            x_polarity = runif(1, -1, 1),
            y_polarity = runif(1, -1, 1),
            x_iRNG = sample(2:2, size = 1),
            y_iRNG = sample(2:2, size = 1),
            x_trigonometricFunction = "Cosecant",
            y_trigonometricFunction = "Cosecant",
            x = quote(x_polarity * x_i^x_iRNG - csc(y_i^sample(-2:-1, size =1)) * x_i^sample(-1:-1, size =1)),
            y = quote(y_polarity * y_i^y_iRNG - csc(x_i^sample(-2:-1, size =1)) * y_i^sample(-1:-1, size =1))
        )
    }
    else if (rng >= 6 && rng <= 10){
        print("trait black dwarf supernova")
        list(
            trait = "Black Dwarf Supernova",
            x_polarity = runif(1, -1, 1),
            y_polarity = runif(1, -1, 1),
            x_iRNG = sample(2:5, size = 1),
            y_iRNG = sample(3:5, size = 1),
            x_trigonometricFunction = "Arccotangent",
            y_trigonometricFunction = "Arccotangent",
            x = quote(x_polarity * x_i^x_iRNG - acot(y_i^sample(2:10, size = 1))),
            y = quote(y_polarity * y_i^y_iRNG - acot(x_i^sample(2:10, size = 1)) * y_i^sample(1:4, size = 1))
        )
    }
    else if (rng >= 11 && rng <= 15){
        print("trait supernova")
        list(
            trait = "Supernova",
            x_polarity = runif(1, -1, 1),
            y_polarity = runif(1, -1, 1),
            x_iRNG = sample(2:5, size = 1),
            y_iRNG = sample(3:5, size = 1),
            x_trigonometricFunction = "Arcsecant",
            y_trigonometricFunction = "Arcsecant",
            x = quote(x_polarity * x_i^x_iRNG - asec(y_i^sample(2:20, size = 1))),
            y = quote(y_polarity * y_i^y_iRNG - acsc(x_i^sample(2:20, size = 1)) * y_i^sample(1:4, size = 1))
        )
    }
    else if (rng >= 16 && rng <= 18){
        print("trait Ring of Fire")
        list(
            trait = "Ring of Fire",
            x_polarity = runif(1, -1, 1),
            y_polarity = runif(1, -1, 1),
            x_iRNG = sample(5:5, size = 1),
            y_iRNG = sample(5:5, size = 1),
            x_trigonometricFunction = "Sine",
            y_trigonometricFunction = "Cosine",
            x = quote(runif(1, -1, -1) * x_i^5 - sin(y_i^2)),
            y = quote(runif(1, -1, -1) * y_i^5 - cos(x_i^2)))
    }
    else if (rng >= 19 && rng <= 23){
        print("trait Stellar Corona")
        list(
            trait = "Stellar Corona",
            x_polarity = runif(1, -1, 1),
            y_polarity = runif(1, -1, 1),
            x_iRNG = sample(2:5, size = 1),
            y_iRNG = sample(3:5, size = 1),
            x_trigonometricFunction = "Tangent",
            y_trigonometricFunction = "Tangent",
            x = quote(x_polarity * x_i^x_iRNG - tan(y_i^sample(-2:0, size = 1))),
            y = quote(y_polarity * y_i^y_iRNG - tan(x_i^sample(-2:0, size = 1)) * y_i^sample(-5:0, size = 1))
        )
    }
    else if (rng >= 24 && rng <= 28){
        print("trait diamond ring")
        list(
            trait = "Diamond Ring",
            x_polarity = runif(1, -1, 1),
            y_polarity = runif(1, -1, 1),
            x_iRNG = 2,
            y_iRNG = 2,
            x_trigonometricFunction = "Tangent",
            y_trigonometricFunction = "Sine",
            x = quote(x_polarity * x_i^x_iRNG - tan(y_i^sample(50:50, size = 1)) * x_i^sample(4:4, size = 1)),
            y = quote(y_polarity * y_i^y_iRNG - sin(x_i^sample(50:50, size = 1)) * y_i^sample(4:4, size = 1))
        )
    }
    else if (rng >= 29 && rng <= 33){
         print("trait baily's beads")
        list(
            trait = "Baily's Beads",
            x_polarity = runif(1, -1, 1),
            y_polarity = runif(1, -1, 1),
            x_iRNG = sample(2:5, size = 1),
            y_iRNG = sample(3:5, size = 1),
            x_trigonometricFunction = "Tangent",
            y_trigonometricFunction = "Tangent",
            x = quote(x_polarity * x_i^x_iRNG - tan(y_i^sample(1:20, size = 1))),
            y = quote(y_polarity * y_i^y_iRNG - tan(x_i^sample(1:2, size = 1)) * y_i^sample(1:50, size = 1))
        )
    }
    else if (rng >= 34 && rng <= 38){
        print("trait New Moon")
        list(
            trait = "New Moon",
            x_polarity = runif(1, -1, 1),
            y_polarity = runif(1, -1, 1),
            x_iRNG = 2,
            y_iRNG = 3,
            x_trigonometricFunction = "Sine",
            y_trigonometricFunction = "Cosine",
            x = quote(x_polarity * x_i^x_iRNG - sin(y_i^sample(-3:-1, size = 1))),
            y = quote(y_polarity * y_i^y_iRNG - cos(x_i^0) * y_i^4)
        )
    }
    else if (rng >= 39 && rng <= 43){
        print("trait gravity")
        list(
            trait = "Gravity",
            x_polarity = runif(1, -1, 1),
            y_polarity = runif(1, -1, 1),
            x_iRNG = 2,
            y_iRNG = 2,
            x_trigonometricFunction = "Sine",
            y_trigonometricFunction = "Cosine",
            x = quote(x_polarity * x_i^x_iRNG - sin(y_i^sample(5:5, size = 1))),
            y = quote(y_polarity * y_i^y_iRNG - cos(x_i^sample(5:5, size = 1)))
        )
    }
    else if (rng >= 44 && rng <= 45){
        print("trait displacement (of star)")
        list(
            trait = "Displacement of Star",
            x_polarity = runif(1, -1, 1),
            y_polarity = runif(1, -1, 1),
            x_iRNG = sample(2:5, size = 1),
            y_iRNG = sample(3:5, size = 1),
            x_trigonometricFunction = "Sine",
            y_trigonometricFunction = "Cosine",
            x = quote(x_polarity * x_i^x_iRNG - sin(y_i^sample(-2:0, size = 1)) * x_i^sample(-4:0, size = 1)),
            y = quote(y_polarity * y_i^y_iRNG - cos(x_i^sample(-2:0, size = 1)) * y_i^sample(-5:0, size = 1))
        )
    }
    else if (rng >= 46 && rng <= 50){
        print("trait prominence")
         list(
            trait = "Solar Prominence",
            x_polarity = runif(1, -1, 1),
            y_polarity = runif(1, -1, 1),
            x_iRNG = sample(2:5, size = 1),
            y_iRNG = sample(3:5, size = 1),
            x_trigonometricFunction = "Sine",
            y_trigonometricFunction = "Cosine",
            x = quote(x_polarity * x_i^x_iRNG - sin(y_i^sample(0:20, size = 1)) * x_i^sample(0:4, size = 1)),
            y = quote(y_polarity * y_i^y_iRNG - cos(x_i^sample(0:2, size = 1)) * y_i^sample(0:50, size = 1))
        )
    }
    else if (rng >= 51 && rng <= 60){
        print("trait Totality Eclipse")
        list(
            trait = "Totality Eclipse",
            x_polarity = runif(1, -1, 1),
            y_polarity = runif(1, -1, 1),
            x_iRNG = sample(2:5, size = 1),
            y_iRNG = sample(3:5, size = 1),
            x_trigonometricFunction = "Sine",
            y_trigonometricFunction = "Cosine",
            x = quote(x_polarity * x_i^x_iRNG - sin(y_i^sample(10:20, size = 1))),
            y = quote(y_polarity * y_i^y_iRNG - cos(x_i^sample(1:2, size = 1)) * y_i^sample(1:50, size = 1))
        )
    }
    else if (rng >= 61 && rng <= 65){
        subRNG <- sample(1:100, size = 1)
        if(subRNG <= 50){
            print("trait Cosmic Magnetism")
            list(
                trait = "Cosmic Magnetism",
                x_polarity = runif(1, -1, 1),
                y_polarity = runif(1, -1, 1),
                x_iRNG = 2,
                y_iRNG = 2,
                x_trigonometricFunction = "Sine",
                y_trigonometricFunction = "Sine",
                x = quote(x_polarity * x_i^x_iRNG - sin(y_i^sample(0:2, size = 1)) * x_i^sample(1:4, size = 1)),
                y = quote(y_polarity * y_i^y_iRNG - sin(x_i^sample(0:2, size = 1)) * y_i^sample(1:4, size = 1))
            )
        } else {
            print("trait Cosmic Magnetism2")
            list(
                trait = "Cosmic Magnetism",
                x_polarity = runif(1, -1, 1),
                y_polarity = runif(1, -1, 1),
                x_iRNG = 2,
                y_iRNG = 2,
                x_trigonometricFunction = "Cosine",
                y_trigonometricFunction = "Cosine",
                x = quote(x_polarity * x_i^x_iRNG - cos(y_i^sample(0:2, size = 1)) * x_i^sample(1:4, size = 1)),
                y = quote(y_polarity * y_i^y_iRNG - cos(x_i^sample(0:2, size = 1)) * y_i^sample(1:4, size = 1))
            )
        }
    }
    else if (rng >= 66 && rng <= 70){
        print("trait Heliocentric Orbit")
        list(
            trait = "Heliocentric Orbit",
            x_polarity = runif(1, -1, 1),
            y_polarity = runif(1, -1, 1),
            x_iRNG = 2,
            y_iRNG = 3,
            x_trigonometricFunction = "Sine",
            y_trigonometricFunction = "Cosine",
            x = quote(x_polarity * x_i^x_iRNG - sin(y_i^sample(0:10, size = 1))),
            y = quote(y_polarity * y_i^y_iRNG - cos(x_i^0) * y_i^4)
        )
    }
    # ARCHIVE=======================
    else if (rng >= 71 && rng <= 80){
        print("trait form")
         list(
            trait = "Form",
            x_polarity = runif(1, -1, 1),
            y_polarity = runif(1, -1, 1),
            x_iRNG = sample(2:3, size = 1),
            y_iRNG = sample(2:3, size = 1),
            x_trigonometricFunction = "Sine",
            y_trigonometricFunction = "Cosine",
            x = quote(x_polarity * x_i^x_iRNG - sin(y_i^sample(-2:2, size =1))),
            y = quote(y_polarity * y_i^y_iRNG - cos(x_i^sample(-2:2, size =1)) * y_i^sample(-1:4, size =1))
        )
    }
    else if (rng >= 81 && rng <= 90){
        print("trait birth")
        list(
            trait = "Birth",
            x = quote(x_polarity * x_i^sample(2:3, size =1) - sin(y_i^sample(-2:2, size =1)) * x_i^sample(-1:4, size =1)),
            y = quote(y_polarity * y_i^sample(2:3, size =1) - cos(x_i^sample(-2:2, size =1)) * y_i^sample(-1:4, size =1))
        )
    }
    else if (rng >= 91 && rng <= 100){
        print("trait morph")
        list(
            trait = "Morph",
            x = quote(x_polarity * x_i^sample(2:2, size = 1) - sin(y_i^sample(0:2, size = 1)) * x_i^sample(0:4, size =1)),
            y = quote(y_polarity * y_i^sample(3:3, size = 1) - cos(x_i^sample(0:2, size = 1)) * y_i^sample(0:4, size = 1))
        )
    }

     # else if (rng >= 16 && rng <= 18){
        # print("trait Hyades star")
        # list(
        #     trait = "Hyades Star",
        #     x_polarity = runif(1, -1, 1),
        #     y_polarity = runif(1, -1, 1),
        #     x_iRNG = sample(2:5, size = 1),
        #     y_iRNG = sample(2:5, size = 1),
        #     x_trigonometricFunction = "Secant",
        #     y_trigonometricFunction = "Secant",
        #     x = quote(x_polarity * x_i^x_iRNG - sec(y_i^sample(1:1, size = 1))),
        #     y = quote(y_polarity * y_i^y_iRNG - sec(x_i^sample(1:1, size = 1)) * y_i^sample(0:0, size = 1))
        # )
    # }

}

gen_plot <- function(df, file_name, x_polarity, y_polarity, filetype, color = "black", background_color = "white", glowRNG, traitName) {    
#     if(traitName == "Ring of Fire" || traitName == "Sirius" || traitName == "Stellar Collision"){
#         x_1 <- 0
#         x_2 <- 0
#         x_3 <- 0
#         x_4 <- 0
#         x_5 <- 0
#         x_6 <- 0
#         x_7 <- 0
#         x_8 <- 0
#         x_9 <- 0
#         x_10 <- 0
#         x_11 <- 0
#         x_12 <- 0
#         x_13 <- 0
#         x_14 <- 0
#         x_15 <- 0
#         x_16 <- 0
#         x_17 <- 0
#         x_18 <- 0
#         x_19 <- 0
#         x_20 <- 0
#    } else
    if(x_polarity >= 0 && y_polarity >= 0){
        x_1 <- -0.1
        x_2 <- -0.2
        x_3 <- -0.3
        x_4 <- -0.4
        x_5 <- -0.5
        x_6 <- -0.6
        x_7 <- -0.7
        x_8 <- -0.8
        x_9 <- -0.9
        x_10 <- -1.0
        x_11 <- -1.1
        x_12 <- -1.2
        x_13 <- -1.3
        x_14 <- -1.4
        x_15 <- -1.5
        x_16 <- -1.6
        x_17 <- -1.7
        x_18 <- -1.8
        x_19 <- -1.9
        x_20 <- -2.0
        x_21 <- -2.1
        x_22 <- -2.2
        x_23 <- -2.3
        x_24 <- -2.4
        x_25 <- -2.5
        x_26 <- -2.6
        x_27 <- -2.7
        x_28 <- -2.8
        x_29 <- -2.9
        x_30 <- -3.0
        x_31 <- -3.1
        x_32 <- -3.2
        x_33 <- -3.3
        x_34 <- -3.4
        x_35 <- -3.5
        x_36 <- -3.6
        x_37 <- -3.7
        x_38 <- -3.8
        x_39 <- -3.9
        x_40 <- -4.0
        x_41 <- -4.1
        x_42 <- -4.2
        x_43 <- -4.3
        x_44 <- -4.4
        x_45 <- -4.5
        x_46 <- -4.6
        x_47 <- -4.7
        x_48 <- -4.8
        x_49 <- -4.9
        x_50 <- -5.0

    } else if (x_polarity <= 0 && y_polarity <= 0){
        x_1 <- 0.1
        x_2 <- 0.2
        x_3 <- 0.3
        x_4 <- 0.4
        x_5 <- 0.5
        x_6 <- 0.6
        x_7 <- 0.7
        x_8 <- 0.8
        x_9 <- 0.9
        x_10 <- 1.0
        x_11 <- 1.1
        x_12 <- 1.2
        x_13 <- 1.3
        x_14 <- 1.4
        x_15 <- 1.5
        x_16 <- 1.6
        x_17 <- 1.7
        x_18 <- 1.8
        x_19 <- 1.9
        x_20 <- 2.0
        x_21 <- 2.1
        x_22 <- 2.2
        x_23 <- 2.3
        x_24 <- 2.4
        x_25 <- 2.5
        x_26 <- 2.6
        x_27 <- 2.7
        x_28 <- 2.8
        x_29 <- 2.9
        x_30 <- 3.0
        x_31 <- 3.1
        x_32 <- 3.2
        x_33 <- 3.3
        x_34 <- 3.4
        x_35 <- 3.5
        x_36 <- 3.6
        x_37 <- 3.7
        x_38 <- 3.8
        x_39 <- 3.9
        x_40 <- 4.0
        x_41 <- 4.1
        x_42 <- 4.2
        x_43 <- 4.3
        x_44 <- 4.4
        x_45 <- 4.5
        x_46 <- 4.6
        x_47 <- 4.7
        x_48 <- 4.8
        x_49 <- 4.9
        x_50 <- 5.0
    } 
    else {
        x_1 <- 0.1
        x_2 <- 0.2
        x_3 <- 0.3
        x_4 <- 0.4
        x_5 <- 0.5
        x_6 <- 0.6
        x_7 <- 0.7
        x_8 <- 0.8
        x_9 <- 0.9
        x_10 <- 1.0
        x_11 <- 1.1
        x_12 <- 1.2
        x_13 <- 1.3
        x_14 <- 1.4
        x_15 <- 1.5
        x_16 <- 1.6
        x_17 <- 1.7
        x_18 <- 1.8
        x_19 <- 1.9
        x_20 <- 2.0
        x_21 <- 2.1
        x_22 <- 2.2
        x_23 <- 2.3
        x_24 <- 2.4
        x_25 <- 2.5
        x_26 <- 2.6
        x_27 <- 2.7
        x_28 <- 2.8
        x_29 <- 2.9
        x_30 <- 3.0
        x_31 <- 3.1
        x_32 <- 3.2
        x_33 <- 3.3
        x_34 <- 3.4
        x_35 <- 3.5
        x_36 <- 3.6
        x_37 <- 3.7
        x_38 <- 3.8
        x_39 <- 3.9
        x_40 <- 4.0
        x_41 <- 4.1
        x_42 <- 4.2
        x_43 <- 4.3
        x_44 <- 4.4
        x_45 <- 4.5
        x_46 <- 4.6
        x_47 <- 4.7
        x_48 <- 4.8
        x_49 <- 4.9
        x_50 <- 5.0
    }


#    if(traitName == "Ring of Fire"){
#         x_1 <- 0
#         x_2 <- 0
#         x_3 <- 0
#         x_4 <- 0
#    }
    # if(rotation == "Right"){
        # x_1 <- 0.1
        # x_2 <- 0.2
        # x_3 <- 0.3
        # x_4 <- 0.4
    # }else {
        # x_1 <- -0.1
        # x_2 <- -0.2
        # x_3 <- -0.3
        # x_4 <- -0.4
    # }

    if (color == "#630200" || 
    (glowRNG >= 51 && (traitName == "Stellar Collision" || traitName == "Sirius" || traitName == "Baily's Beads" || traitName == "New Moon" || traitName == "Displacement of Star" || traitName == "Totality Eclipse" || traitName == "Ring of Fire"))) {
        plot <- df %>%
            ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 1, x = x, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 2, x = x+x_1, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 3, x = x+x_2, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 4, x = x+x_3, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 5, x = x+x_4, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 6, x = x+x_5, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 7, x = x+x_6, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 8, x = x+x_7, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 9, x = x+x_8, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 10, x = x+x_9, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 11, x = x+x_10, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 12, x = x+x_11, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 13, x = x+x_12, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 14, x = x+x_13, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 15, x = x+x_14, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 16, x = x+x_15, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 17, x = x+x_16, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 18, x = x+x_17, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 19, x = x+x_18, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 20, x = x+x_19, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 21, x = x+x_20, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 22, x = x+x_21, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 23, x = x+x_22, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 24, x = x+x_23, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 25, x = x+x_24, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 26, x = x+x_25, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 27, x = x+x_26, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 28, x = x+x_27, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 29, x = x+x_28, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 30, x = x+x_29, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 31, x = x+x_30, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 32, x = x+x_31, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 33, x = x+x_32, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 34, x = x+x_33, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 35, x = x+x_34, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 36, x = x+x_35, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 37, x = x+x_36, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 38, x = x+x_37, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 39, x = x+x_38, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 40, x = x+x_39, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 41, x = x+x_40, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 42, x = x+x_41, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 43, x = x+x_42, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 44, x = x+x_43, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 45, x = x+x_44, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 46, x = x+x_45, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 47, x = x+x_46, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 48, x = x+x_47, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 49, x = x+x_48, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 50, x = x+x_49, y = y)), colour = color, sigma = glowRNG ) +
            with_outer_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 51, x = x+x_50, y = y)), colour = color, sigma = glowRNG ) +


            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 1, x = x, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 2, x = x+x_1, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 3, x = x+x_2, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 4, x = x+x_3, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 5, x = x+x_4, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 6, x = x+x_5, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 7, x = x+x_6, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 8, x = x+x_7, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 9, x = x+x_8, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 10, x = x+x_9, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 11, x = x+x_10, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 12, x = x+x_11, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 13, x = x+x_12, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 14, x = x+x_13, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 15, x = x+x_14, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 16, x = x+x_15, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 17, x = x+x_16, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 18, x = x+x_17, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 19, x = x+x_18, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 20, x = x+x_19, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 21, x = x+x_20, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 22, x = x+x_21, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 23, x = x+x_22, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 24, x = x+x_23, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 25, x = x+x_24, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 26, x = x+x_25, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 27, x = x+x_26, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 28, x = x+x_27, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 29, x = x+x_28, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 30, x = x+x_29, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 31, x = x+x_30, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 32, x = x+x_31, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 33, x = x+x_32, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 34, x = x+x_33, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 35, x = x+x_34, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 36, x = x+x_35, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 37, x = x+x_36, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 38, x = x+x_37, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 39, x = x+x_38, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 40, x = x+x_39, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 41, x = x+x_40, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 42, x = x+x_41, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 43, x = x+x_42, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 44, x = x+x_43, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 45, x = x+x_44, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 46, x = x+x_45, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 47, x = x+x_46, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 48, x = x+x_47, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 49, x = x+x_48, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 50, x = x+x_49, y = y)), colour = color, sigma = glowRNG ) +
            with_inner_glow( ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 51, x = x+x_50, y = y)), colour = color, sigma = glowRNG ) +


            ggplot2::theme_void() +
            ggplot2::coord_fixed() +
            ggplot2::coord_polar() +
            ggplot2::theme(
                panel.background = element_rect(fill = background_color),
                plot.background = element_rect(fill = background_color)
            )
    
    } else {
         plot <- df %>%
            ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 1, x = x, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 2, x = x+x_1, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 3, x = x+x_2, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 4, x = x+x_3, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 5, x = x+x_4, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 6, x = x+x_5, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 7, x = x+x_6, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 8, x = x+x_7, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 9, x = x+x_8, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 10, x = x+x_9, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 11, x = x+x_10, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 12, x = x+x_11, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 13, x = x+x_12, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 14, x = x+x_13, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 15, x = x+x_14, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 16, x = x+x_15, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 17, x = x+x_16, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 18, x = x+x_17, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 19, x = x+x_18, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 20, x = x+x_19, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 21, x = x+x_20, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 22, x = x+x_21, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 23, x = x+x_22, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 24, x = x+x_23, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 25, x = x+x_24, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 26, x = x+x_25, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 27, x = x+x_26, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 28, x = x+x_27, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 29, x = x+x_28, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 30, x = x+x_29, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 31, x = x+x_30, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 32, x = x+x_31, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 33, x = x+x_32, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 34, x = x+x_33, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 35, x = x+x_34, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 36, x = x+x_35, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 37, x = x+x_36, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 38, x = x+x_37, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 39, x = x+x_38, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 40, x = x+x_39, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 41, x = x+x_40, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 42, x = x+x_41, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 43, x = x+x_42, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 44, x = x+x_43, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 45, x = x+x_44, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 46, x = x+x_45, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 47, x = x+x_46, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 48, x = x+x_47, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 49, x = x+x_48, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 50, x = x+x_49, y = y)) +
            ggplot2::geom_point(alpha = 0.1, size = 0, shape = 20, color = color, aes(frame = 51, x = x+x_50, y = y)) +
            ggplot2::theme_void() +
            ggplot2::coord_fixed() +
            ggplot2::coord_polar() +
            ggplot2::theme(
                panel.background = element_rect(fill = background_color),
                plot.background = element_rect(fill = background_color)
            )
    }

    # the != is designs that dont fit in any scale
    if(traitName != "Stellar Collision" && traitName != "Cosmic Magnetism" && traitName != "Solar Prominence" && traitName != "Displacement of Star" && traitName != "New Moon" && traitName != "Diamond Ring" && traitName != "Ring of Fire" && traitName != "Sirius" && traitName != "Stellar Corona"){
        print("scale")
    #     if(traitName == "Baily's Beads"){
            # nice for really round designs
            # plot <- plot + ggplot2::scale_x_continuous(trans='log10') 
            # plot <- plot + ggplot2::scale_x_continuous(trans='log1p') 
        # }else{
            # nice for really round designs
            plot <- plot + ggplot2::scale_x_continuous(trans='sqrt') 

            # plot <- plot +scale_x_reverse()
            # plot <- plot + scale_y_reverse()
    }
    # }
    # else{
    #     plot <- plot + ggplot2::scale_x_discrete()
    # }
    
    gganimate(plot, filename = paste0(IMG_PATH, file_name), ani.width = 1800, ani.height = 1800, title_frame = FALSE, interval = 0.05)
    gc()
    # ggplot2::ggsave(plot, filename = paste0(IMG_PATH, file_name), width = 6, height = 6, device = filetype)
}

gen_data <- function(formula) {
    # change the dark and lightness of the art
    df <- seq(from = -pi, to = pi, by = 0.01) %>%
        expand.grid(x_i = ., y_i = .) %>%
        dplyr::mutate(!!!formula)
    return(df)
}
gen_seeds <- function(nr_of_img) {
    seeds <- sample(1:100000, nr_of_img)
}

generate_logfile_entry <- function(logfile, index, background_color, color, formula, seed, file_name, colorTraitName, glowRNG) {
    # print(paste0("index: ", index))
    # print(paste0("trait: ", formula["trait"]))
    # # print(paste0("bgColorTraitName: ", bgColorTraitName))
    # print(paste0("colorTraitName: ", colorTraitName))
    # print(paste0("formula: ", formula))
    # print(paste0("Probability: ", seed))
    # print(paste0("glowTraitName: ", glowTraitName))
    # print( JSON_metadata)
    # creating the data for JSON file
    
    # writing into JSON file
    if(formula["x_polarity"] >= 0 && formula["y_polarity"] >= 0){
        polarityTraitName <- "Positive"
    } else if (formula["x_polarity"] <= 0 && formula["y_polarity"] <= 0){
        polarityTraitName <- "Negative"
    } else {
        polarityTraitName <- "Balanced"
    }

    if(formula["x_iRNG"] >= 0 && formula["x_iRNG"] <= 2 && formula["y_iRNG"] >= 0 && formula["y_iRNG"] <= 2){
        density <- "High"
    } else if (formula["x_iRNG"] > 20 && formula["y_iRNG"] > 20){
        density <- "Low"
    } else if (formula["x_iRNG"] > 2 && formula["y_iRNG"] > 2){
        density <- "Average"
    }
     else if (formula["x_iRNG"] < 0 && formula["y_iRNG"] < 0){
        density <- "Super"
    } else {
        density <- "Balanced"
    }

    if(glowRNG >= 51 && glowRNG <= 98 ){
        glowTraitName <- "Shiny"
    } else {
        glowTraitName <- "Legendary"
    }

    if ( glowRNG >= 51 && (formula["trait"] == "Stellar Collision" || formula["trait"] == "Sirius" || formula["trait"] == "Baily's Beads" || formula["trait"] == "New Moon" || formula["trait"] == "Displacement of Star" || formula["trait"] == "Totality Eclipse")) {
        logfile_tmp <- data.frame(
            index = c(index),
            Probability = paste0("1 in ", seed),
            Series = c(formula["trait"]),
            # backgroundColor = c(bgColorTraitName),
            Color = c(colorTraitName),
            Polarity = c(polarityTraitName),
            # Rotation = c(rotation),
            Density = c(density),
            X_Trigonometric_Function = formula["x_trigonometricFunction"],
            Y_Trigonometric_Function = formula["y_trigonometricFunction"],
            Glow = glowTraitName
        )  
    } else {
        logfile_tmp <- data.frame(
            index = c(index),
            Probability = paste0("1 in ", seed),
            Series = c(formula["trait"]),
            # backgroundColor = c(bgColorTraitName),
            Color = c(colorTraitName),
            Polarity = c(polarityTraitName),
            # Rotation = c(rotation),
            Density = c(density),
            X_Trigonometric_Function = formula["x_trigonometricFunction"],
            Y_Trigonometric_Function = formula["y_trigonometricFunction"]
        )
    }

    
    # logfile <- dplyr::bind_rows(logfile, logfile_tmp)
    write.table(logfile_tmp, LOGFILE_PATH,sep = ",", quote = F, row.names = F, append = TRUE, col.names=!file.exists(LOGFILE_PATH))
    print("logfile saved")
}

gen_filename <- function(index, filetype) {
    print(index)
    file_name <- paste0(index, ".", filetype)
}

generate_cutterKom <- function(formula, nr_of_img, polar = FALSE, filetype = "png", seed, color,
                               background_color, index, colorTraitName, glowRNG) {
    # seeds <- generate_seeds(nr_of_img)
    # purrr::map(seeds, function(seed){

    # if(formula["x_polarity"] >= 0 && formula["y_polarity"] >= 0){
    #    rotation <- "Left"
    # } else if (formula["x_polarity"] <= 0 && formula["y_polarity"] <= 0){
    #    rotation <- "Right"
    # } else {
    #    rotation <- "None"
    # }
    set.seed(seed)
    file_name <- gen_filename(index, filetype)
    logfile <- check_logfile_existence()
    logfile <- generate_logfile_entry(logfile, index, background_color, color, formula, seed, file_name, colorTraitName, glowRNG)
    df <- gen_data(formula)
    plot <- gen_plot(
        df, file_name, formula["x_polarity"], formula["y_polarity"], filetype, color,
        background_color, glowRNG, formula["trait"])
    # })
}
seeds <- gen_seeds(nr_of_img)
purrr::imap(seeds, function(seed, index) {
    bgcolorRNG <- runif(1, 1, 100)
    colorRNG <- runif(1, 1, 100)

    if(bgcolorRNG <= 98){
        color <- "#FFFFFD"
        colorTraitName <- "Solar"
        background_color <- "#051427"
        # background_color <- "#000000"
    }
    # else if (bgcolorRNG >= 91 && bgcolorRNG <= 98){
    #     colorTraitName <- "Lunar"
        # background_color <- "#0c0000"
    #     # color <- "#630200"
    #     color <- "#e66d39"
    # }
     else{
        colorTraitName <- "Lunar"
        background_color <- "#130000"
        color <- "#630200"
        
        # color <- "#E84600"
    }

   
    # if(colorRNG <= 60){
    #     colorTraitName <- "Solar"
    #     color <- "#FFFFFD"
    #     # glowColor <- "#FFFFFD"
    # }else if(colorRNG >= 61 && colorRNG <= 80){
    #     colorTraitName <- "Ring of Fire"
    #     color <- "#FFFFFD"
    #     # glowColor <- "#E84600"
    # } else{
    #     colorTraitName <- "Blood Moon"
    #     color <- "#630200"
    #     # glowColor <- "#630200"
    # }

    glowRNG <- runif(1, 0, 100)

    # if(glowRNG <= 2.5){
    #     glowTraitName <- "Low"
    # } else if (glowRNG >= 2.6 && glowRNG <= 5 ){
    #     glowTraitName <- "Medium"
    # } else if (glowRNG <= 5.1 && glowRNG >= 7.5 ){
    #     glowTraitName <- "Medium"
    # } else{
    #     glowTraitName <- "High"
    # }



    generate_cutterKom(formula = getFomula(), nr_of_img, polar = TRUE, filetype = "gif", seed, color, background_color, index, colorTraitName, glowRNG)
})

# jsonData <- toJSON(JSON_metadata, force = TRUE)
# print("jsonData")
# print(jsonData)
# write(jsonData, "metadata.json") 