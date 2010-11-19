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
package org.apache.myfaces.extensions.validator.core.metadata.extractor;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import java.util.Map;

/**
 * The interface for all factories which create meta-data extractors.
 * 
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public interface ComponentMetaDataExtractorFactory
{
    /**
     * Returns a MetaDataExtractor. The factory should cache the result instance for performance reasons.
     * @return MetaDataExtractor.
     */
    MetaDataExtractor create();

    /**
     * Returns a MetaDataExtractor that add the supplied properties to the PropertyInformation object during extraction
     * of the information.
     *
     * @param properties Properties to add to the information.
     * @return MetaDataExtractor.
     */
    MetaDataExtractor createWith(Map<String, Object> properties);
}