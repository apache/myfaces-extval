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
package org.apache.myfaces.extensions.validator.core.metadata.transformer.mapper;

import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.CustomInformation;
import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ProxyUtils;

/**
 * It's an alternative Mapper to place ValidationStrategies and MetaDataTransformers in the same package.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@InvocationOrder(400)
@UsageInformation({UsageCategory.INTERNAL})
public class SimpleValidationStrategyToMetaDataTransformerNameMapper extends
    AbstractValidationStrategyToMetaDataTransformerNameMapper
{
    public String createName(ValidationStrategy validationStrategy)
    {
        Class<? extends ValidationStrategy> validationStrategyClass =
                ProxyUtils.getUnproxiedClass(validationStrategy.getClass(), ValidationStrategy.class);

        if(validationStrategyClass.getPackage() == null)
        {
            return null;
        }
        return getSimpleMetaDataTransformerName(validationStrategyClass.getPackage().getName() + ".",
                                                validationStrategyClass.getSimpleName());
    }

    public String getSimpleMetaDataTransformerName(String validationStrategyPackageName,
                                                 String validationStrategyClassName)
    {
        String postfix = ExtValContext.getContext().getInformationProviderBean()
            .get(CustomInformation.META_DATA_TRANSFORMER_POSTFIX);

        if(validationStrategyClassName.endsWith("ValidationStrategy") ||
           validationStrategyClassName.endsWith("Strategy"))
        {
            return validationStrategyPackageName + validationStrategyClassName
                    .replace(ExtValContext.getContext().getInformationProviderBean()
                        .get(CustomInformation.VALIDATION_STRATEGY_POSTFIX) ,postfix)
                    .replace("ValidationStrategy", postfix)
                    .replace("Strategy", postfix);
        }

        //in case of a static validation strategy mapping
        return validationStrategyPackageName + validationStrategyClassName + postfix;
    }

}