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
 * {@link org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer MetaDataTransformers}
 * are used to convert specific information of constraints to a generic representation.
 * So it's possible to transform different meta-data implementations to an independent representation.
 * E.g. @Length and @Size specifies the same information.
 * {@link org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer MetaDataTransformers}
 * are aware of the concrete meta-data implementation but they aren't aware of JSF components.
 *
 * The result of the transformation is used by
 * {@link org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer ComponentInitializers}
 * to initialize JSF components based on the found meta-data.
 *
 * {@link org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys} provides the keys used by ExtVal.
 *
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
//*Transformer instead of *Converter to avoid naming confusion 
public interface MetaDataTransformer
{
    /**
     * Converts the information of a {@link MetaDataEntry} into an independent format.
     *
     * @param metaData The meta-data which should be converted.
     * @return Map with the converted information.
     */
    Map<String, Object> convertMetaData(MetaDataEntry metaData);
}
