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
package org.apache.myfaces.extensions.validator.core.factory;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * Class mapping factories create a result based on a given instance.
 * Most factories use the {@link org.apache.myfaces.extensions.validator.core.mapper.NameMapper} facility
 * to determine the type of the result.
 *
 * @param <P> source type
 * @param <R> target type

 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public interface ClassMappingFactory<P, R>
{
    /**
     * Creates an instance of the target type based on the source type.
     * Example:
     * {@link org.apache.myfaces.extensions.validator.core.metadata.transformer.DefaultMetaDataTransformerFactory}
     * creates a {@link org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer}
     * for a given {@link org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy}.
     *
     * @param source source object
     * @return target object.
     */
    R create(P source);
}
