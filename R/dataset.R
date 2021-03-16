#' Dog breed dataset
#'
#' Prepares the Dog Breed dataset available in Kaggle [here](https://www.kaggle.com/c/dog-breed-identification)
#'
#' We use Kaggle API CLI to download the dataset.If you want to download the dataset.
#'
#' @param root path to the data location.
#' @param split string. 'train' or 'submission'.
#' @param transform function that receives a torch tensor and return another torch tensor, transformed.
#' @param download wether to download or not.
#' @param force force download?
#' @param loader function. first argument is path for image. See [torchvision::base_loader].
#'
#' @export
dog_breed_dataset <- torch::dataset(
  "DogBreed",
  classes = c("boston_bull", "dingo", "pekinese", "bluetick", "golden_retriever",
              "bedlington_terrier", "borzoi", "basenji", "scottish_deerhound",
              "shetland_sheepdog", "walker_hound", "maltese_dog", "norfolk_terrier",
              "african_hunting_dog", "wire_haired_fox_terrier", "redbone",
              "lakeland_terrier", "boxer", "doberman", "otterhound", "standard_schnauzer",
              "irish_water_spaniel", "black_and_tan_coonhound", "cairn", "affenpinscher",
              "labrador_retriever", "ibizan_hound", "english_setter", "weimaraner",
              "giant_schnauzer", "groenendael", "dhole", "toy_poodle", "border_terrier",
              "tibetan_terrier", "norwegian_elkhound", "shih_tzu", "irish_terrier",
              "kuvasz", "german_shepherd", "greater_swiss_mountain_dog", "basset",
              "australian_terrier", "schipperke", "rhodesian_ridgeback", "irish_setter",
              "appenzeller", "bloodhound", "samoyed", "miniature_schnauzer",
              "brittany_spaniel", "kelpie", "papillon", "border_collie", "entlebucher",
              "collie", "malamute", "welsh_springer_spaniel", "chihuahua",
              "saluki", "pug", "malinois", "komondor", "airedale", "leonberg",
              "mexican_hairless", "bull_mastiff", "bernese_mountain_dog", "american_staffordshire_terrier",
              "lhasa", "cardigan", "italian_greyhound", "clumber", "scotch_terrier",
              "afghan_hound", "old_english_sheepdog", "saint_bernard", "miniature_pinscher",
              "eskimo_dog", "irish_wolfhound", "brabancon_griffon", "toy_terrier",
              "chow", "flat_coated_retriever", "norwich_terrier", "soft_coated_wheaten_terrier",
              "staffordshire_bullterrier", "english_foxhound", "gordon_setter",
              "siberian_husky", "newfoundland", "briard", "chesapeake_bay_retriever",
              "dandie_dinmont", "great_pyrenees", "beagle", "vizsla", "west_highland_white_terrier",
              "kerry_blue_terrier", "whippet", "sealyham_terrier", "standard_poodle",
              "keeshond", "japanese_spaniel", "miniature_poodle", "pomeranian",
              "curly_coated_retriever", "yorkshire_terrier", "pembroke", "great_dane",
              "blenheim_spaniel", "silky_terrier", "sussex_spaniel", "german_short_haired_pointer",
              "french_bulldog", "bouvier_des_flandres", "tibetan_mastiff",
              "english_springer", "cocker_spaniel", "rottweiler"),

  initialize = function(root, split = "train", transform = NULL, download = FALSE, loader = torchvision::base_loader, force = FALSE) {

    self$transform <- transform
    self$loader <- loader
    self$split <- split

    # donwload ----------------------------------------------------------
    competition <- "dog-breed-identification"
    data_path <- normalizePath(fs::path(root, competition))
    if(fs::dir_exists(data_path) && force)
      fs::dir_delete(data_path)
    if (!fs::dir_exists(data_path) && download) {
      zipfile <- fs::path(root, paste0(competition, ".zip"))

      system(glue::glue("kaggle competitions download -c {competition} -p {root}"))
      unzip(zipfile, exdir = data_path)
      file.remove(zipfile)
    }

    if (!fs::dir_exists(data_path))
      stop("No data found. Please use `download = TRUE`.")

    # variavel resposta -------------------------------------------------

    if(split == "train") {
      images <- read.csv(fs::path(data_path, "labels.csv"))
      images$breed <- factor(clean_string(images$breed), levels = self$classes)
      self$images <- images
      self$.path <- file.path(data_path, "train")
    } else if(split == "submission") {
      images <- read.csv(fs::path(data_path, "sample_submission.csv"))
      images$breed <- NA_character_
      names(images) <- clean_string(names(images))
      self$images <- images[c("id", self$classes)]
      self$.path <- file.path(data_path, "test")
    }
  },

  .getitem = function(index) {
    force(index)
    sample <- self$images[index, ]

    id <- sample$id
    y <- as.integer(sample$breed)
    label <- as.character(sample$breed)
    x <- self$loader(file.path(self$.path, paste0(id, ".jpg")))

    if (!is.null(self$transform))
      x <- self$transform(x)

    return(list(x = x, y = y, id = id, label = label))
  },

  .length = function() {
    nrow(self$images)
  }
)

