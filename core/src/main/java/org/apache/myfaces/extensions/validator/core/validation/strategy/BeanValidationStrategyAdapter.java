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

import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

/**
 * Adapter to connect validation strategies with meta-data transformers,
 * if the validation strategy is defined as bean and e.g. spring creates a proxy.
 *
 * it isn't linked to jsr 303.
 *
 * it's just a helper for proxies - you just need it, if you define the validation strategy as bean and
 * e.g. spring creates a proxy for it.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@ToDo(value = Priority.HIGH, description = "see EXTVAL-116")
@UsageInformation({UsageCategory.REUSE})
public interface BeanValidationStrategyAdapter extends ValidationStrategy
{
    //to get back the internal caching
    /**
     * Must return the class name of ValidationStrategy wrapped by this adapter.
     *
     * @return class name of the wrapped ValidationStrategy
     */
    String getValidationStrategyClassName();

    /**
     * Must return the class name of the MetaDataTransformer for the wrapped ValidationStrategy of the adapter.
     *
     * @return class name of the MetaDataTransformer for the wrapped ValidationStrategy.
     */
    String getMetaDataTransformerClassName();
}
