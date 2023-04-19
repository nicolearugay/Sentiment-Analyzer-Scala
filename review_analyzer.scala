// https://github.com/nicolearugay
// Uploaded 4/18/2023

import com.github.tototoshi.csv._
import java.io.File

object ReviewAnalyzer2 extends App {
  // Read the input file
  val fileName = "path/to/csv"

  // Open the CSV file using the CSVReader library
  val reader = CSVReader.open(new File(fileName))
  // Read all lines and drop the header row
  val lines = reader.all().drop(1)

  // Define a case class for the review data
  case class Review(productId: String, score: Int, text: String)

  // Map each line to a Review instance
  val reviews = lines.map { line =>
    val columns = line.toArray
    Review(columns(1), columns(6).toInt, columns(9))
  }

  // Define a function to categorize the sentiment based on the score
  def sentiment(score: Int): String = {
    if (score > 3) "positive"
    else if (score < 3) "negative"
    else "neutral"
  }

  // Get the unique product IDs
  val productIds = reviews.map(_.productId).distinct

  // Prepare a list to store the results
  val results = productIds.map { productId =>
    val filteredReviews = reviews.filter(_.productId == productId)
    val sentimentCounts = filteredReviews.groupBy(review => sentiment(review.score)).view.mapValues(_.size).toMap
    val totalReviews = filteredReviews.size

    val positivePercentage = sentimentCounts.getOrElse("positive", 0) * 100.0 / totalReviews
    val negativePercentage = sentimentCounts.getOrElse("negative", 0) * 100.0 / totalReviews
    val neutralPercentage = sentimentCounts.getOrElse("neutral", 0) * 100.0 / totalReviews

    // Return the result as a Map
    Map(
      "Product ID" -> productId,
      "Positive" -> f"$positivePercentage%1.2f%%",
      "Negative" -> f"$negativePercentage%1.2f%%",
      "Neutral" -> f"$neutralPercentage%1.2f%%"
    )
  }

  // Save the results to a CSV file
  val outputFileName = "output.csv"
  val outputFile = new File(outputFileName)
  val csvWriter = CSVWriter.open(outputFile)
  csvWriter.writeRow(List("Product ID", "Positive", "Negative", "Neutral"))
  results.foreach(result => csvWriter.writeRow(result.values.toList))
  csvWriter.close()
}


