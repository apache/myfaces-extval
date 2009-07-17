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
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.DefaultComponentMetaDataExtractorFactory;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.CustomInformation;
import org.apache.myfaces.extensions.validator.core.storage.DefaultStorageManagerFactory;
import org.apache.myfaces.extensions.validator.core.el.DefaultELHelperFactory;
import org.apache.myfaces.extensions.validator.core.renderkit.DefaultRenderKitWrapperFactory;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.DefaultMetaDataTransformerFactory;
import org.apache.myfaces.extensions.validator.core.validation.strategy.DefaultValidationStrategyFactory;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.DefaultMessageResolverFactory;
import org.apache.myfaces.extensions.validator.core.validation.message.DefaultFacesMessageFactory;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterExtractorFactory;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

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
    protected final Log logger = LogFactory.getLog(getClass());
    protected Map<FactoryNames, Object> factoryMap = new HashMap<FactoryNames, Object>();

    private static FactoryFinder factoryFinder = new DefaultFactoryFinder();

    private DefaultFactoryFinder()
    {
        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

    public static FactoryFinder getInstance()
    {
        return factoryFinder;
    }

    @SuppressWarnings({"unchecked"})
    public final <T> T getFactory(FactoryNames factoryName, Class<T> targetClass)
    {
        if(!(factoryMap.containsKey(factoryName)))
        {
            initFactory(factoryName);
        }

        return (T)factoryMap.get(factoryName);
    }

    private void initFactory(FactoryNames factoryName)
    {
        Object factory = null;
        switch (factoryName)
        {
            case COMPONENT_META_DATA_EXTRACTOR_FACTORY:
                factory = createComponentMetaDataExtractorFactory();
                break;

            case VALIDATION_STRATEGY_FACTORY:
                factory = createValidationStrategyFactory();
                break;

            case MESSAGE_RESOLVER_FACTORY:
                factory = createMessageResolverFactory();
                break;

            case META_DATA_TRANSFORMER_FACTORY:
                factory = createMetaDataTransformerFactory();
                break;

            case RENDERKIT_WRAPPER_FACTORY:
                factory = createRenderKitWrapperFactory();
                break;

            case EL_HELPER_FACTORY:
                factory = createELHelperFactory();
                break;

            case FACES_MESSAGE_FACTORY:
                factory = createFacesMessageFactory();
                break;

            case VALIDATION_PARAMETER_EXTRACTOR_FACTORY:
                factory = createValidationParameterExtractorFactory();
                break;

            case STORAGE_MANAGER_FACTORY:
                factory = createStorageManagerFactory();
                break;
             
            default: //required by checkstyle
        }

        if(factory == null)
        {
            throw new IllegalStateException("not possible to create factory " + factoryName);
        }

        factoryMap.put(factoryName, factory);
    }

    @ToDo(value = Priority.MEDIUM, description = "add global property extension point")
    protected Object createComponentMetaDataExtractorFactory()
    {
        Object factory = null;

        List<String> metaDataExtractorFactoryClassNames = new ArrayList<String>();

        metaDataExtractorFactoryClassNames.add(WebXmlParameter.CUSTOM_COMPONENT_META_DATA_EXTRACTOR_FACTORY);
        metaDataExtractorFactoryClassNames
            .add(ExtValContext.getContext().getInformationProviderBean()
                .get(CustomInformation.COMPONENT_META_DATA_EXTRACTOR_FACTORY));
        metaDataExtractorFactoryClassNames.add(DefaultComponentMetaDataExtractorFactory.class.getName());

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

    @ToDo(value = Priority.MEDIUM, description = "add global property extension point")
    protected Object createValidationStrategyFactory()
    {
        Object factory = null;

        List<String> validationStrategyFactoryClassNames = new ArrayList<String>();

        validationStrategyFactoryClassNames.add(WebXmlParameter.CUSTOM_VALIDATION_STRATEGY_FACTORY);
        validationStrategyFactoryClassNames
            .add(ExtValContext.getContext().getInformationProviderBean()
                    .get(CustomInformation.VALIDATION_STRATEGY_FACTORY));
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

    @ToDo(value = Priority.MEDIUM, description = "add global property extension point")
    protected Object createMessageResolverFactory()
    {
        Object factory = null;
        List<String> messageResolverFactoryClassNames = new ArrayList<String>();

        messageResolverFactoryClassNames.add(WebXmlParameter.CUSTOM_MESSAGE_RESOLVER_FACTORY);
        messageResolverFactoryClassNames
            .add(ExtValContext.getContext().getInformationProviderBean()
                    .get(CustomInformation.MESSAGE_RESOLVER_FACTORY));
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

    @ToDo(value = Priority.MEDIUM, description = "add global property extension point")
    protected Object createMetaDataTransformerFactory()
    {
        Object factory = null;
        List<String> metaDataTransformerFactoryClassNames = new ArrayList<String>();

        metaDataTransformerFactoryClassNames.add(WebXmlParameter.CUSTOM_META_DATA_TRANSFORMER_FACTORY );
        metaDataTransformerFactoryClassNames
            .add(ExtValContext.getContext().getInformationProviderBean()
                    .get(CustomInformation.META_DATA_TRANSFORMER_FACTORY));
        metaDataTransformerFactoryClassNames.add(DefaultMetaDataTransformerFactory.class.getName());

        for (String className : metaDataTransformerFactoryClassNames)
        {
            factory = ClassUtils.tryToInstantiateClassForName(className);

            if (factory != null)
            {
                break;
            }
        }

        return factory;
    }

    protected Object createRenderKitWrapperFactory()
    {
        return new DefaultRenderKitWrapperFactory();
    }

    @ToDo(value = Priority.MEDIUM, description = "add global property extension point")
    protected Object createFacesMessageFactory()
    {
        Object factory = null;

        List<String> facesMessageFactoryClassNames = new ArrayList<String>();

        facesMessageFactoryClassNames.add(WebXmlParameter.CUSTOM_FACES_MESSAGE_FACTORY);
        facesMessageFactoryClassNames
            .add(ExtValContext.getContext().getInformationProviderBean()
                    .get(CustomInformation.FACES_MESSAGE_FACTORY));

        Object target = ExtValContext.getContext().getGlobalProperty(CustomInformation.FACES_MESSAGE_FACTORY.name());
        if(target != null && target instanceof String)
        {
            facesMessageFactoryClassNames.add((String)target);
        }
        facesMessageFactoryClassNames.add(DefaultFacesMessageFactory.class.getName());

        for (String className : facesMessageFactoryClassNames)
        {
            factory = ClassUtils.tryToInstantiateClassForName(className);

            if (factory != null)
            {
                break;
            }
        }

        return factory;
    }

    protected Object createELHelperFactory()
    {
        return new DefaultELHelperFactory();
    }

    @ToDo(value = Priority.MEDIUM, description = "add global property extension point")
    protected Object createValidationParameterExtractorFactory()
    {
        Object factory = null;

        List<String> validationParameterExtractorFactoryClassNames = new ArrayList<String>();

        validationParameterExtractorFactoryClassNames
                .add(WebXmlParameter.CUSTOM_VALIDATION_PARAMETER_EXTRACTOR_FACTORY);
        validationParameterExtractorFactoryClassNames
            .add(ExtValContext.getContext().getInformationProviderBean()
                .get(CustomInformation.VALIDATION_PARAMETER_EXTRACTOR_FACTORY));
        validationParameterExtractorFactoryClassNames.add(DefaultValidationParameterExtractorFactory.class.getName());

        for (String className : validationParameterExtractorFactoryClassNames)
        {
            factory = ClassUtils.tryToInstantiateClassForName(className);

            if (factory != null)
            {
                break;
            }
        }
        return factory;
    }

    protected Object createStorageManagerFactory()
    {
        return new DefaultStorageManagerFactory();
    }
}
