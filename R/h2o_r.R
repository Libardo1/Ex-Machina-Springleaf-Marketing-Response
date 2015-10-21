h2o.shim(enable = TRUE)

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
if (! ("methods"  %in% rownames(installed.packages()))) { install.packages("methods") }
if (! ("statmod"  %in% rownames(installed.packages()))) { install.packages("statmod") }
if (! ("stats"    %in% rownames(installed.packages()))) { install.packages("stats") }
if (! ("graphics" %in% rownames(installed.packages()))) { install.packages("graphics") }
if (! ("RCurl"    %in% rownames(installed.packages()))) { install.packages("RCurl") }
if (! ("jsonlite" %in% rownames(installed.packages()))) { install.packages("jsonlite") }
if (! ("tools"    %in% rownames(installed.packages()))) { install.packages("tools") }
if (! ("utils"    %in% rownames(installed.packages()))) { install.packages("utils") }

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-slater/5/R")))
library("h2o")
local_h2o_cluster = h2o.init(max_mem_size = '3g')

## Import Data to H2O Cluster
train_hex <- h2o.importFile(local_h2o_cluster, "/Users/KCUser/ExMachina/Kagglespace/African_Soil_Property_Prediction/data/train.zip")
test_hex <- h2o.importFile(local_h2o_cluster, "/Users/KCUser/ExMachina/Kagglespace/African_Soil_Property_Prediction/data/test.zip")

## Split the dataset into 80:20 for training and validation
train_hex_split <- h2o.splitFrame(train_hex, ratios = 0.8)

## One Variable at at Time
ls_label <- c("Ca", "P", "pH", "SOC", "Sand")

for (n_label in 1:5) {
  
  ## Display
  cat("\n\nNow training a DNN model for", ls_label[n_label], "...\n")
  
  ## Train a 50-node, three-hidden-layer Deep Neural Networks for 100 epochs
  model <- h2o.deeplearning(x = 2:3595,
                            y = (3595 + n_label),
                            training_frame = train_hex_split[[1]],
                            validation = train_hex_split[[2]],
                            activation = "Rectifier",
                            hidden = c(50, 50, 50),
                            epochs = 100,
                            classification = FALSE,
                            balance_classes = FALSE)
  
  ## Print the Model Summary
  print(model)
  
  ## Use the model for prediction and store the results in submission template
  raw_sub[, (n_label + 1)] <- as.matrix(h2o.predict(model, test_hex))
  
}
