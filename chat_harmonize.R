library(ellmer)
library(jsonlite)

Sys.setenv(OPENAI_API_KEY = "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")
# Start a chat with ChatGPT
chat <- chat_openai(
  api_key = Sys.getenv("OPENAI_API_KEY")
)

# Start a chat with ChatGPT
chat <- chat_openai(model = "gpt-4o", api_key = Sys.getenv("OPENAI_API_KEY"))

# Construct a prompt
prompt <- paste0(
  "Match each of the following submitted value names to the most likely value from this master list. Please provide your answers in a tabular format",
  paste(accepted_vars, collapse = ", "),
  ".\n\nSubmitted values:\n",
  paste(submitted_vars, collapse = "\n")
)

# Ask the model
response <- chat$chat(prompt)


# Example usage
accepted_vars <- ghgi_variables$variable
submitted_vars <- c("greenhouse gas", "Economic Sector", "us_state_or_territory")

accepted_vars <- ghgi_values$value
submitted_vars <- c("carbon_dioxide", "carbon dioxide from FFC", "arizona", 
                    "arkinsaw", "production of Zn", "hfc 236 cb", "ww discharge",
                    "crop_land_conv_forested", "burn ag residue", "wetl_rem_wetl", 
                    "NEU-FF")
