

# LLMs need tools to do things other than chatting (e.g., do math)
chat$register_tool(tool(function() Sys.Date(), 
                        .description = "gets the current date"))

chat$chat("What is today's date?")

# Note that every chat saves the entirety of the chat, 
# resending it to the LLM each time a new prompt is added. 

# Structured Data

