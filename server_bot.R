# initial -----------------------------------------------------------------
# library(lubridate)
library(telegram.bot)
# library(tidyverse)

bot <- Bot(token = scan("/var/temperature.bot.token", what = "", quiet = TRUE))

# custom functions --------------------------------------------------------
get.temp <- function(){ 
    a <- read.table(
        "./temperature.csv", 
        header = TRUE,
        sep = ",") |>
        dplyr::mutate(tm = lubridate::ydm_hms(tm)) |> 
        tidyr::pivot_longer(names_to = "cpu", values_to = "tmp", -tm) |>
        dplyr::group_by(tm) |> 
        dplyr::summarise(sd = sd(tmp), tmp = mean(tmp))
    a
    }

viz.temp <- function(x = "0", df = get.temp()){ 
    
    if(x != "0"){
        x <-  as.numeric(x)
        x <- round(x*60)
        df <- df |>
            dplyr::filter(dplyr::between(tm, max(tm) - lubridate::minutes(x), max(tm)))
    }
    
    ggplot2::ggplot(df, ggplot2::aes(tm, tmp, ymin = tmp-sd, ymax = tmp+sd)) + 
        ggplot2::geom_ribbon(color = NA, fill = "lightgrey") +
        ggplot2::geom_line() + 
        ggplot2::labs(
            x = NULL, 
            y = "Temperature, °C",
            subtitle = Sys.time() |> substr(1, 16) |> stringr::str_replace_all(" ", "\n")
        ) + 
        ggplot2::theme_classic() 
        # ggplot2::scale_x_datetme(date_breaks = "1 hour", date_labels =  "%h:%m, %d %m")
    
    
    }


# global.ip0 <- function(){system("curl ident.me", intern = TRUE)}
global.ip <- function() {
    httr::GET("http://ifconfig.me/ip") |> 
        httr::content("text") |> 
        suppressMessages() 
}
local.ip <- function(){system("hostname -I", intern = TRUE)}
mac <- function(){ system("ip link", intern = TRUE) %>% 
        str_subset("ether") %>% 
        str_split(" ") %>% 
        .[[1]] %>% 
        str_subset("\\:") %>% 
        .[[1]]
    # paste(global.ip, local.ip, mac)
}

get.speed <- function(){
    sp <- system("speedtest", intern = TRUE)
    paste0(sp[c(7, 9)], collapse = "<br>")
    
}


# Bot handlers ------------------------------------------------------------
fun_net <- function(bot, update){ 
    bot$sendMessage(
        chat_id = update$message$chat_id, 
        text = paste0(
            "<b>External IP: </b>", global.ip(),
            "\n<b>Internal IP: </b>", local.ip(),
            "\n<b>MAC address: </b>", mac()
        ),
        parse_mode = "HTML"
    )
}

fun_info <- function(bot, update){
    df <- get.temp()
    up <- system("uptime", intern = TRUE) |> 
        str_split_1(" min") %>% 
        .[1] %>% 
        str_split_1("up ") %>% 
        .[2] %>% 
        str_split_1("\\:") 
    if(length(up)<2){up <- c(0, 0, up)}
    if(length(up)<3){up <- c(0, up)}
    bot$sendMessage(
        # chat_id = 276388547,
        chat_id = update$message$chat_id,
        text = paste0(
            "<b>Log stats </b>\n \n<b>", nrow(df), 
            "</b> records \n<b>from:</b> ", 
            min(df$tm), 
            "\n<b>to:</b> ", 
            max(df$tm), 
            "\n \nPeak temperature: <b>", 
            round(max(df$tmp), 1),
            " °C</b> \nMean temperature: <b>", 
            round(mean(df$tmp), 1),
            " °C</b>",
            "\n\n<b>Server uptime:</b> \n", up[1], " days ", up[2], " hours ", up[3], " mins\n"
        ),
        parse_mode = "HTML"
    )
}

fun_viz <- function(bot, update){
    df <- get.temp()
    nm = paste0("/home/orangepi/monitor/export_", str_replace_all(Sys.time(), "\\:", "."), ".png")
    ggsave(nm, plot = viz.temp(), height = 5, width = 4, dpi = 600)
    
    bot$sendPhoto(
        # chat_id = 276388547,
        chat_id = update$message$chat_id,
        photo = nm,
        caption = paste0(
            "<b>", nrow(df), 
            "</b> records \n<b>from:</b> ", 
            min(df$tm), 
            "\n<b>to:</b> ", 
            max(df$tm), 
            "\n \nPeak temperature: <b>", 
            round(max(df$tmp), 1),
            " °C</b> \nMean temperature: <b>", 
            round(mean(df$tmp), 1),
            " °C</b>"
        ),
        parse_mode = "HTML"
    )
}

fun_start <- function(bot, update){ 
    df <- get.temp()
    
    bot$sendMessage(
        # chat_id = update$message$chat_id, 
        chat_id = 276388547,
        text = paste0(
            "<b>Telegram bot for manage my Orange Pi5 server</b>\n\nNet parameters\n<b>External IP: </b>", global.ip(),
            "\n<b>Internal IP: </b>", local.ip(),
            "\n<b>MAC address: </b>", mac(), 
            "\n\nTemperature logs\n<b>", nrow(df), 
            "</b> records \n<b>from:</b> ", 
            min(df$tm), 
            "\n<b>to:</b> ", 
            max(df$tm), 
            "\nPeak temperature: <b>", 
            round(max(df$tmp), 1),
            " °C</b> \nMean temperature: <b>", 
            round(mean(df$tmp), 1),
            " °C</b>\n\nOther commands: /info /net /viz"
            ),
        parse_mode = "HTML"
    )
}

# welcome message ---------------------------------------------------------
up <- system("uptime", intern = TRUE) |> 
    str_split_1(" min") %>% 
    .[1] %>% 
    str_split_1("up ") %>% 
    .[2] %>% 
    str_split_1("\\:") 
if(length(up)<2){up <- c(0, 0, up)}
if(length(up)<3){up <- c(0, up)}
bot$sendMessage(chat_id = 276388547, 
    text = paste0(
        "<b>Bot is started at: </b>\n", Sys.time(), "\n",
        "\n<b>Server uptime:</b> \n", up[1], " days ", up[2], " hours ", up[3], " mins\n",
        "\n<b>External IP: </b>", global.ip(),
        "\n<b>Internal IP: </b>", local.ip(),
        "\n<b>MAC address: </b>", mac(), "\n", 
        "\nCommands available: /info /net /viz"
    ),
    parse_mode = "HTML"
)


# run bot -----------------------------------------------------------------
updater <- Updater(token = scan("/var/temperature.bot.token", what = "", quiet = TRUE)) + 
    CommandHandler("start", fun_start)+
    CommandHandler("net", fun_net) + 
    CommandHandler("viz", fun_viz) + 
    CommandHandler("info", fun_info)

updater$start_polling()
