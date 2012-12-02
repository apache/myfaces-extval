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
package org.apache.myfaces.extensions.validator.core.validation.strategy;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

/**
 * if an adapter (ValidationStrategy only used for component initialization) is used for several constraints, this
 * interface allows to identify instances.
 * For the moment only used for JSR-303 validation strategies.
 * 
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public interface IdentifiableValidationStrategy extends ValidationStrategy
{
    /**
     * Separator used in the unique key that identifies validationStrategyName when multiple constraints are processed
     * by a MetaDataTransformer.
     * @see org.apache.myfaces.extensions.validator.core.metadata.transformer.DefaultMetaDataTransformerFactory
     */
    String ID_PREFIX = ":";

    /**
     * Returns the unique part of the key of the  validationStrategyName when multiple constraints are processed
     * by a MetaDataTransformer.
     * @return Unique String to identify the Validation.
     */
    String getId();
}
