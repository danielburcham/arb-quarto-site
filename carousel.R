library(htmltools)
library(yaml)

carousel <- function(id, duration, items) {
    index <- -1
    items <- lapply(items, function(item) {
        index <<- index + 1
        carouselItem(item$caption, item$image, item$link, index, duration)
    })
    
    items <- div(class = "carousel-inner",
                 tagList(lapply(items, function(item) item$item))           
    )
    div(id = id, class="carousel carousel-dark slide", `data-bs-ride`="carousel",
        items,
        navButton(id, "prev", "Prevoius"),
        navButton(id, "next", "Next")
    )
}

carouselItem <- function(caption, image, link, index, interval) {
    id <- paste0("gallery-carousel-item-", index)
    button <- tags$button(type = "button", 
                          `data-bs-target` = "#gallery-carousel",
                          `data-bs-slide-to` = index,
                          `aria-label` = paste("Slide", index + 1)
    )
    if (index == 0) {
        button <- tagAppendAttributes(button,
                                      class = "active",
                                      `aria-current` = "true"                  
        )
    }
    item <- div(class = paste0("carousel-item", ifelse(index == 0, " active", "")),
                `data-bs-interval` = interval,
                a(href = link, img(src = image, class = "d-block  mx-auto border")),
                div(class = "carousel-caption d-none d-md-block",
                    tags$p(class = "fw-light", caption)
                )
    )
    list(
        button = button,
        item = item
    )
}

navButton <- function(targetId, type, text) {
    tags$button(class = paste0("carousel-control-", type),
                type = "button",
                `data-bs-target` = paste0("#", targetId),
                `data-bs-slide` = type,
                span(class = paste0("carousel-control-", type, "-icon"),
                     `aria-hidden` = "true"),
                span(class = "visually-hidden", text)
    )
}