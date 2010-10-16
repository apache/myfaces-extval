/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.core.metadata.transformer;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;

import java.util.Map;

/**
 * Allows to transform concrete meta-data to a more abstract form.<br/>
 * e.g.: @Required, @Column(nullable = false,...), @Length(minimum = 1), ... -> required = true  <br/>
 * For the keys of the map the constants in the CommonMetaDataKeys can be used.
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
//*Transformer instead of *Converter to avoid naming confusion 
public interface MetaDataTransformer
{
    /**
     * Convert the information from MetaDataEntry into the abstract format.
     * @param metaData The MetaDataEntry to process.
     * @return Map with the information abstract format. 
     */
    Map<String, Object> convertMetaData(MetaDataEntry metaData);
}
