# =========================================================================
# Copyright © 2019 T-Mobile USA, Inc.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# =========================================================================

# Set an endpoint to tell GCP the app is ready to serve
#* @get /readiness_check
readiness_check<- function(){
  return ("app ready")
}

# Set an endpoint to tell GCP the app is alive
#* @get /liveness_check
liveness_check<- function(){
  return ("app live")
}

# source('api-solver.R')

# Set an endpoint to return makusafe score
# @post /maku
# the_score <- function(input){
#   get_score(input)
# }
