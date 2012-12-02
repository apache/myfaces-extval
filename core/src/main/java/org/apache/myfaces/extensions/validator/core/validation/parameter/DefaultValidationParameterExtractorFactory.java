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
package org.apache.myfaces.extensions.validator.core.validation.parameter;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.CustomInformation;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.util.ClassUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

/**
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultValidationParameterExtractorFactory implements ValidationParameterExtractorFactory
{
    private final Logger logger = Logger.getLogger(getClass().getName());

    private static ValidationParameterExtractor validationParameterExtractor = null;

    public DefaultValidationParameterExtractorFactory()
    {
        logger.fine(getClass().getName() + " instantiated");
    }

    public ValidationParameterExtractor create()
    {
        if (validationParameterExtractor == null)
        {
            List<String> validationParameterExtractorClassNames = new ArrayList<String>();

            validationParameterExtractorClassNames
                    .add(ExtValCoreConfiguration.get().customValidationParameterExtractorClassName());
            validationParameterExtractorClassNames
                .add(ExtValContext.getContext().getInformationProviderBean()
                    .get(CustomInformation.VALIDATION_PARAMETER_EXTRACTOR));
            validationParameterExtractorClassNames.add(DefaultValidationParameterExtractor.class.getName());

            for (String className : validationParameterExtractorClassNames)
            {
                validationParameterExtractor = (ValidationParameterExtractor)
                        ClassUtils.tryToInstantiateClassForName(className);

                if (validationParameterExtractor != null)
                {
                    break;
                }
            }
        }

        logger.finest(validationParameterExtractor.getClass().getName() + " created");

        return validationParameterExtractor;
    }
}