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

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.core.annotation.extractor.DefaultComponentAnnotationExtractorFactory;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.renderkit.DefaultRenderKitWrapperFactory;
import org.apache.myfaces.extensions.validator.core.initializer.rendering.DefaultRenderingContextInitializerFactory;
import org.apache.myfaces.extensions.validator.core.initializer.component.DefaultComponentInitializerFactory;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.DefaultMetaDataExtractorFactory;
import org.apache.myfaces.extensions.validator.core.validation.strategy.DefaultValidationStrategyFactory;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.DefaultMessageResolverFactory;
import org.apache.myfaces.extensions.validator.util.ClassUtils;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
//dynamic approach to create the factories during the first request, when a faces-context is available
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultFactoryFinder implements FactoryFinder
{
    Map<FactoryNameEnum, Object> factoryMap = new HashMap<FactoryNameEnum, Object>();

    public <T> T getFactory(FactoryNameEnum factoryNameEnum, Class<T> targetClass)
    {
        if(!(factoryMap.containsKey(factoryNameEnum)))
        {
            initFactory(factoryNameEnum);
        }

        return (T)factoryMap.get(factoryNameEnum);
    }

    private void initFactory(FactoryNameEnum factoryName)
    {
        Object factory = null;
        switch (factoryName)
        {
            case COMPONENT_ANNOTATION_EXTRACTOR_FACTORY:
                factory = createComponentAnnotationExtractorFactory();
                break;

            case VALIDATION_STRATEGY_FACTORY:
                factory = createValidationStrategyFactory();
                break;

            case MESSAGE_RESOLVER_FACTORY:
                factory = createMessageResolverFactory();
                break;

            case META_DATA_EXTRACTOR_FACTORY:
                factory = createMetaDataExtractorFactory();
                break;

            case COMPONENT_INITIALIZER_FACTORY:
                factory = createComponentInitializerFactory();
                break;

            case RENDERKIT_WRAPPER_FACTORY:
                factory = createRenderKitWrapperFactory();
                break;

            case RENDERING_CONTEXT_INITIALIZER_FACTORY:
                factory = createRenderingContextInitializerFactory();
                break;
            
            default: //required by checkstyle
        }

        if(factory == null)
        {
            throw new IllegalStateException("not possible to create factory " + factoryName);
        }

        factoryMap.put(factoryName, factory);
    }

    private Object createComponentAnnotationExtractorFactory()
    {
        Object factory = null;

        List<String> annotationExtractorFactoryClassNames = new ArrayList<String>();

        annotationExtractorFactoryClassNames.add(WebXmlParameter.CUSTOM_COMPONENT_ANNOTATION_EXTRACTOR_FACTORY);
        annotationExtractorFactoryClassNames
            .add(ExtValContext.getContext().getInformationProviderBean()
                .getCustomComponentAnnotationExtractorFactory());
        annotationExtractorFactoryClassNames.add(DefaultComponentAnnotationExtractorFactory.class.getName());

        for (String className : annotationExtractorFactoryClassNames)
        {
            factory = ClassUtils.tryToInstantiateClassForName(className);

            if (factory != null)
            {
                break;
            }
        }
        return factory;
    }

    private Object createValidationStrategyFactory()
    {
        Object factory = null;

        List<String> validationStrategyFactoryClassNames = new ArrayList<String>();

        validationStrategyFactoryClassNames.add(WebXmlParameter.CUSTOM_VALIDATION_STRATEGY_FACTORY);
        validationStrategyFactoryClassNames
            .add(ExtValContext.getContext().getInformationProviderBean().getCustomValidationStrategyFactory());
        validationStrategyFactoryClassNames.add(DefaultValidationStrategyFactory.class.getName());

        for (String className : validationStrategyFactoryClassNames)
        {
            factory = ClassUtils.tryToInstantiateClassForName(className);

            if (factory != null)
            {
                break;
            }
        }

        return factory;
    }

    private Object createMessageResolverFactory()
    {
        Object factory = null;
            List<String> messageResolverFactoryClassNames = new ArrayList<String>();

            messageResolverFactoryClassNames.add(WebXmlParameter.CUSTOM_MESSAGE_RESOLVER_FACTORY);
            messageResolverFactoryClassNames
                .add(ExtValContext.getContext().getInformationProviderBean().getCustomMessageResolverFactory());
            messageResolverFactoryClassNames
                .add(DefaultMessageResolverFactory.class.getName());

            for (String className : messageResolverFactoryClassNames)
            {
                factory = ClassUtils.tryToInstantiateClassForName(className);

                if (factory != null)
                {
                    break;
                }
            }

        return factory;
    }

    private Object createMetaDataExtractorFactory()
    {
        Object factory = null;
            List<String> metaDataExtractorFactoryClassNames = new ArrayList<String>();

            metaDataExtractorFactoryClassNames.add(WebXmlParameter.CUSTOM_META_DATA_EXTRACTOR_FACTORY);
            metaDataExtractorFactoryClassNames
                .add(ExtValContext.getContext().getInformationProviderBean().getCustomMetaDataExtractorFactory());
            metaDataExtractorFactoryClassNames.add(DefaultMetaDataExtractorFactory.class.getName());

            for (String className : metaDataExtractorFactoryClassNames)
            {
                factory = ClassUtils.tryToInstantiateClassForName(className);

                if (factory != null)
                {
                    break;
                }
            }

        return factory;
    }

    private Object createComponentInitializerFactory()
    {
        Object factory = null;
        List<String> componentInitializerFactoryClassNames = new ArrayList<String>();

        componentInitializerFactoryClassNames.add(WebXmlParameter.CUSTOM_COMPONENT_INITIALIZER_FACTORY);
        componentInitializerFactoryClassNames
            .add(ExtValContext.getContext().getInformationProviderBean().getCustomComponentInitializerFactory());
        componentInitializerFactoryClassNames.add(DefaultComponentInitializerFactory.class.getName());

        for (String className : componentInitializerFactoryClassNames)
        {
            factory = ClassUtils.tryToInstantiateClassForName(className);

            if (factory != null)
            {
                break;
            }
        }

        return factory;
    }

    private Object createRenderKitWrapperFactory()
    {
        return new DefaultRenderKitWrapperFactory();
    }

    private Object createRenderingContextInitializerFactory()
    {
        Object factory = null;

            List<String> renderingContextInitializerFactoryClassNames = new ArrayList<String>();

            renderingContextInitializerFactoryClassNames
                .add(WebXmlParameter.CUSTOM_RENDERING_CONTEXT_INITIALIZER_FACTORY);
            renderingContextInitializerFactoryClassNames
                .add(ExtValContext.getContext().getInformationProviderBean()
                    .getCustomRenderingContextInitializerFactory());
            renderingContextInitializerFactoryClassNames
                .add(DefaultRenderingContextInitializerFactory.class.getName());

            for (String className : renderingContextInitializerFactoryClassNames)
            {
                factory = ClassUtils.tryToInstantiateClassForName(className);

                if (factory != null)
                {
                    break;
                }
            }

        return factory;
    }
}