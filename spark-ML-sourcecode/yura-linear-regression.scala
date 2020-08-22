// This is generalized regression model for Yura's Cutting&Crimping Process
// created 2020 03 21
// R2 = -0.007
import org.apache.spark.sql.types._


// Data Schema Definition
val schema = StructType(

     StructField("key", StringType, nullable = true) ::
     StructField("surface", StringType, nullable = true) ::
     StructField("diameter", StringType, nullable = true) ::
     StructField("color", StringType, nullable = true) ::
     StructField("inspectionMachine", StringType, nullable = true) ::
     StructField("terminal", StringType, nullable = true) ::
     StructField("expectedMachine", StringType, nullable = true) ::
     StructField("executedMachine", StringType, nullable = true) ::
     StructField("shift", StringType, nullable = true) ::
     StructField("rework", StringType, nullable = true) ::
     StructField("inspector", StringType, nullable = true) ::
     StructField("defectMold", StringType, nullable = true) ::
     StructField("worktime", StringType, nullable = true) ::
     StructField("workday", StringType, nullable = true) ::
     StructField("testtime", StringType, nullable = true) ::     
     StructField("testday", StringType, nullable = true) ::
     StructField("y_length", DoubleType, nullable = true) ::
     StructField("www_strip1", DoubleType, nullable = true) ::
     StructField("www_strip2", DoubleType, nullable = true) ::
     StructField("w_sabun", StringType, nullable = true) ::
     StructField("w_name", StringType, nullable = true) ::
     StructField("w_dept", StringType, nullable = true) ::
     StructField("w_bdate", StringType, nullable = true) ::
     StructField("w_gender", StringType, nullable = true) ::
     StructField("w_edu", StringType, nullable = true) ::
     StructField("w_hand", StringType, nullable = true) ::
     StructField("w_level", StringType, nullable = true) ::
     StructField("w_multi", StringType, nullable = true) :: 
     StructField("w_domi", StringType, nullable = true) ::  
     StructField("w_house", StringType, nullable = true) ::  
     StructField("w_year", StringType, nullable = true) ::  
     StructField("w_smoking", StringType, nullable = true) ::  
     StructField("w_blood", StringType, nullable = true) ::  
     StructField("w_alchol", StringType, nullable = true) ::  
     StructField("w_marry", StringType, nullable = true) ::  
     StructField("w_baby", StringType, nullable = true) ::  
     StructField("w_preg", StringType, nullable = true) ::  
     StructField("w_transp", StringType, nullable = true) ::  
     StructField("w_commutime", StringType, nullable = true) ::  
     StructField("w_people", StringType, nullable = true) ::  
     StructField("w_pay", DoubleType, nullable = true) ::  
     StructField("w_height", DoubleType, nullable = true) ::  
     StructField("w_weight", DoubleType, nullable = true) ::  
     StructField("ngrate", DoubleType, nullable = true) :: Nil)


// MES data csv data file
val sourceFile = "/Users/gary/Downloads/data_gate.csv"

// read csv format with DataFrame type
val df = spark.read.format("csv").option("header", "true").schema(schema).load(sourceFile)

// printing file schema
df.printSchema
val sourceDF = df.na.drop()

// if you want to use SQL to handle dataset, see below example
sourceDF.createOrReplaceTempView("dfs")
spark.sql("select ngrate from dfs").show


// Split trainingData, testData
val Array(trainingData, testData) = sourceDF.randomSplit(Array(0.7, 0.3))

// importing Factor -> IndexEncoding
// grouping features columns and target label
import org.apache.spark.ml.feature.StringIndexer
import org.apache.spark.ml.feature.VectorAssembler
import org.apache.spark.ml.Pipeline


// I would take nominal variables with indexer
// please refer to https://spark.apache.org/docs/2.1.0/ml-features.html#indextostring

val keyI = new StringIndexer().setInputCol("key").setOutputCol("keyCat").fit(sourceDF)
val surfaceI = new StringIndexer().setInputCol("surface").setOutputCol("surfaceCat").fit(sourceDF)
val diameterI = new StringIndexer().setInputCol("diameter").setOutputCol("diameterCat").fit(sourceDF)
val colorI = new StringIndexer().setInputCol("color").setOutputCol("colorCat").fit(sourceDF)
val inspectionMachineI = new StringIndexer().setInputCol("inspectionMachine").setOutputCol("inspectionMachineCat").fit(sourceDF)
val terminalI = new StringIndexer().setInputCol("terminal").setOutputCol("terminalCat").fit(sourceDF)
val expectedMachineI = new StringIndexer().setInputCol("expectedMachine").setOutputCol("expectedMachineCat").fit(sourceDF)
val executedMachineI = new StringIndexer().setInputCol("executedMachine").setOutputCol("executedMachineCat").fit(sourceDF)
val shiftI = new StringIndexer().setInputCol("shift").setOutputCol("shiftCat").fit(sourceDF)
val reworkI = new StringIndexer().setInputCol("rework").setOutputCol("reworkCat").fit(sourceDF)
val inspectorI = new StringIndexer().setInputCol("inspector").setOutputCol("inspectorCat").fit(sourceDF)
val defectMoldI = new StringIndexer().setInputCol("defectMold").setOutputCol("defectMoldCat").fit(sourceDF)
val worktimeI = new StringIndexer().setInputCol("worktime").setOutputCol("worktimeCat").fit(sourceDF)
val workdayI = new StringIndexer().setInputCol("workday").setOutputCol("workdayCat").fit(sourceDF)
val testtimeI = new StringIndexer().setInputCol("testtime").setOutputCol("testtimeCat").fit(sourceDF)
val testdayI = new StringIndexer().setInputCol("testday").setOutputCol("testdayCat").fit(sourceDF)
val w_sabunI = new StringIndexer().setInputCol("w_sabun").setOutputCol("w_sabunCat").fit(sourceDF)
val w_nameI = new StringIndexer().setInputCol("w_name").setOutputCol("w_nameCat").fit(sourceDF)
val w_deptI = new StringIndexer().setInputCol("w_dept").setOutputCol("w_deptCat").fit(sourceDF)
val w_bdateI = new StringIndexer().setInputCol("w_bdate").setOutputCol("w_bdateCat").fit(sourceDF)
val w_genderI = new StringIndexer().setInputCol("w_gender").setOutputCol("w_genderCat").fit(sourceDF)
val w_eduI = new StringIndexer().setInputCol("w_edu").setOutputCol("w_eduCat").fit(sourceDF)
val w_handI = new StringIndexer().setInputCol("w_hand").setOutputCol("w_handCat").fit(sourceDF)
val w_levelI = new StringIndexer().setInputCol("w_level").setOutputCol("w_levelCat").fit(sourceDF)
val w_multiI = new StringIndexer().setInputCol("w_multi").setOutputCol("w_multiCat").fit(sourceDF)
val w_domiI = new StringIndexer().setInputCol("w_domi").setOutputCol("w_domiCat").fit(sourceDF)
val w_houseI = new StringIndexer().setInputCol("w_house").setOutputCol("w_houseCat").fit(sourceDF)
val w_yearI = new StringIndexer().setInputCol("w_year").setOutputCol("w_yearCat").fit(sourceDF)
val w_smokingI = new StringIndexer().setInputCol("w_smoking").setOutputCol("w_smokingCat").fit(sourceDF)
val w_bloodI = new StringIndexer().setInputCol("w_blood").setOutputCol("w_bloodCat").fit(sourceDF)
val w_alcholI = new StringIndexer().setInputCol("w_alchol").setOutputCol("w_alcholCat").fit(sourceDF)
val w_marryI = new StringIndexer().setInputCol("w_marry").setOutputCol("w_marryCat").fit(sourceDF)
val w_babyI = new StringIndexer().setInputCol("w_baby").setOutputCol("w_babyCat").fit(sourceDF)
val w_pregI = new StringIndexer().setInputCol("w_preg").setOutputCol("w_pregCat").fit(sourceDF)
val w_transpI = new StringIndexer().setInputCol("w_transp").setOutputCol("w_transpCat").fit(sourceDF)
val w_commutimeI = new StringIndexer().setInputCol("w_commutime").setOutputCol("w_commutimeCat").fit(sourceDF)
val w_peopleI = new StringIndexer().setInputCol("w_people").setOutputCol("w_peopleCat").fit(sourceDF)



var lable = "ngrate" // declare labe column

// and you should define "Features" vectors with combination columns, except "key"
val assembler = new VectorAssembler().setInputCols(Array("surfaceCat", "diameterCat", "colorCat", "inspectionMachineCat", "terminalCat", "expectedMachineCat", "executedMachineCat", "shiftCat", "reworkCat", "inspectorCat", "defectMoldCat", "worktimeCat", "workdayCat", "testtimeCat", "testdayCat", "y_length", "www_strip1", "www_strip2", "w_sabunCat", "w_nameCat", "w_deptCat", "w_bdateCat", "w_genderCat", "w_eduCat", "w_handCat", "w_levelCat", "w_multiCat" , "w_domiCat"  , "w_houseCat"  , "w_yearCat"  , "w_smokingCat"  , "w_bloodCat"  , "w_alcholCat"  , "w_marryCat"  , "w_babyCat"  , "w_pregCat"  , "w_transpCat"  , "w_commutimeCat"  , "w_peopleCat"  , "w_pay", "w_height", "w_weight")).setOutputCol("Features")


//import ML
import org.apache.spark.ml.regression.LinearRegression;
val lr = new LinearRegression().setMaxIter(10).setRegParam(0.3).setElasticNetParam(0.8).setLabelCol(lable).setFeaturesCol("Features")


// Chain indexer and forest in a Pipeline.
// please refer to https://spark.apache.org/docs/2.1.0/ml-pipeline.html
// Train model. This also runs the indexer.
val pipeline = new Pipeline().setStages(Array(surfaceI, diameterI, colorI, inspectionMachineI, terminalI, expectedMachineI, executedMachineI, shiftI, reworkI, inspectorI, defectMoldI, worktimeI, workdayI, testtimeI, testdayI , w_sabunI , w_nameI, w_deptI , w_bdateI, w_genderI, w_eduI, w_handI, w_levelI, w_multiI, w_domiI, w_houseI, w_yearI, w_smokingI, w_bloodI, w_alcholI, w_marryI, w_babyI, w_pregI, w_transpI, w_commutimeI, w_peopleI, assembler, lr))

// Train model. This also runs the indexer.
val model = pipeline.fit(trainingData)

// Make predictions.
val predictions = model.transform(testData)


// Select 10 rows to display and compare with real value and predicted value
predictions.select("prediction", lable).show(10)


// Evaluation with RMSE
import org.apache.spark.ml.evaluation.RegressionEvaluator;


val r2Evaluator = new RegressionEvaluator().setLabelCol(lable).setPredictionCol("prediction").setMetricName("r2")

val maeEvaluator = new RegressionEvaluator().setLabelCol(lable).setPredictionCol("prediction").setMetricName("mae")

val rmseEvaluator = new RegressionEvaluator().setLabelCol(lable).setPredictionCol("prediction").setMetricName("rmse")

val r2 = r2Evaluator.evaluate(predictions)
val mae = maeEvaluator.evaluate(predictions)
val rmse = rmseEvaluator.evaluate(predictions)


println("R Square = " + r2)
println("Mean Absolute Error (MAE) on test data = " + mae)
println("Root Mean Square Error (RMSE) on test data = " + rmse)

// Select example rows to display.
//predictions.select("prediction", lable, "features").show(20)

//model.save("/user/spark/rf.model")
import org.jpmml.sparkml.PMMLBuilder
import org.dmg.pmml.Model
import java.io.File
val schema = sourceDF.schema
val pmml = new PMMLBuilder(schema, model)
val file = pmml.buildFile(new File("/Users/gary/Onedrive//lr.pmml"))