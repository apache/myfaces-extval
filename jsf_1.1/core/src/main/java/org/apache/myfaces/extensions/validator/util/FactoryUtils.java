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
package org.apache.myfaces.extensions.validator.util;

import org.apache.myfaces.extensions.validator.core.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.annotation.extractor.AnnotationExtractorFactory;
import org.apache.myfaces.extensions.validator.core.annotation.extractor.DefaultAnnotationExtractorFactory;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.DefaultMessageResolverFactory;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.MessageResolver;
import org.apache.myfaces.extensions.validator.core.validation.strategy.DefaultValidationStrategyFactory;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Gerhard Petracek
 */
//TODO
public class FactoryUtils
{
    private static AnnotationExtractorFactory annotationExtractorFactory;

    public static AnnotationExtractorFactory getAnnotationExtractorFactory()
    {
        if (annotationExtractorFactory == null)
        {
            List<String> annotationExtractorFactoryClassNames = new ArrayList<String>();

            annotationExtractorFactoryClassNames
                .add(WebXmlParameter.CUSTOM_ANNOTATION_EXTRACTOR_FACTORY);
            annotationExtractorFactoryClassNames.add(ExtValUtils
                .getInformationProviderBean()
                .getCustomAnnotationExtractorFactory());
            annotationExtractorFactoryClassNames
                .add(DefaultAnnotationExtractorFactory.class.getName());

            for (String className : annotationExtractorFactoryClassNames)
            {
                annotationExtractorFactory = (AnnotationExtractorFactory) ClassUtils
                    .tryToInstantiateClassForName(className);

                if (annotationExtractorFactory != null)
                {
                    //TODO logging
                    break;
                }
            }
        }

        return annotationExtractorFactory;
    }

    private static ClassMappingFactory<Annotation, ValidationStrategy> validationStrategyFactory;

    public static ClassMappingFactory<Annotation, ValidationStrategy> getValidationStrategyFactory()
    {
        if (validationStrategyFactory == null)
        {
            List<String> validationStrategyFactoryClassNames = new ArrayList<String>();

            validationStrategyFactoryClassNames
                .add(WebXmlParameter.CUSTOM_VALIDATION_STRATEGY_FACTORY);
            validationStrategyFactoryClassNames.add(ExtValUtils
                .getInformationProviderBean()
                .getCustomValidationStrategyFactory());
            validationStrategyFactoryClassNames
                .add(DefaultValidationStrategyFactory.class.getName());

            for (String className : validationStrategyFactoryClassNames)
            {
                validationStrategyFactory = (ClassMappingFactory<Annotation, ValidationStrategy>) ClassUtils
                    .tryToInstantiateClassForName(className);

                if (validationStrategyFactory != null)
                {
                    //TODO logging
                    break;
                }
            }
        }

        return validationStrategyFactory;
    }

    private static ClassMappingFactory<ValidationStrategy, MessageResolver> messageResolverFactory;

    public static ClassMappingFactory<ValidationStrategy, MessageResolver> getMessageResolverFactory()
    {
        if (messageResolverFactory == null)
        {
            List<String> messageResolverFactoryClassNames = new ArrayList<String>();

            messageResolverFactoryClassNames
                .add(WebXmlParameter.CUSTOM_MESSAGE_RESOLVER_FACTORY);
            messageResolverFactoryClassNames.add(ExtValUtils
                .getInformationProviderBean()
                .getCustomMessageResolverFactory());
            messageResolverFactoryClassNames
                .add(DefaultMessageResolverFactory.class.getName());

            for (String className : messageResolverFactoryClassNames)
            {
                messageResolverFactory = (ClassMappingFactory<ValidationStrategy, MessageResolver>) ClassUtils
                    .tryToInstantiateClassForName(className);

                if (messageResolverFactory != null)
                {
                    //TODO logging
                    break;
                }
            }
        }

        return messageResolverFactory;
    }
}
