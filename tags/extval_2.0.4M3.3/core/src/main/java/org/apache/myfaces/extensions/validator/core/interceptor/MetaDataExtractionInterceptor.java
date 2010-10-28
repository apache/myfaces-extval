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
package org.apache.myfaces.extensions.validator.core.interceptor;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.InvocationOrderSupport;

/**
 * Allows to post-process the result of the meta-data extraction process (see
 * {@link org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor})
 *
 * @author Gerhard Petracek
 * @since 1.x.2
 */
@InvocationOrderSupport
@UsageInformation(UsageCategory.API)
public interface MetaDataExtractionInterceptor
{
    /**
     * Allows to execute additional actions on the
     * {@link org.apache.myfaces.extensions.validator.core.property.PropertyInformation}
     * data after the extraction is performed and before it is
     * used to determine validation strategies or UIComponent adjustments.
     *  
     * @param propertyInformation the information entry which contains information about the property
     */
    void afterExtracting(PropertyInformation propertyInformation);
}
