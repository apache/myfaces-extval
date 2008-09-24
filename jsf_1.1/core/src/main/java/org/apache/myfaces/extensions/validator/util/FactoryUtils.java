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
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.DefaultMetaDataExtractorFactory;
import org.apache.myfaces.extensions.validator.core.annotation.extractor.AnnotationExtractorFactory;
import org.apache.myfaces.extensions.validator.core.annotation.extractor.DefaultComponentAnnotationExtractorFactory;
import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.core.initializer.component.DefaultComponentInitializerFactory;
import org.apache.myfaces.extensions.validator.core.initializer.rendering.DefaultRenderingContextInitializerFactory;
import org.apache.myfaces.extensions.validator.core.initializer.rendering.RenderingContextInitializer;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.DefaultMessageResolverFactory;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.MessageResolver;
import org.apache.myfaces.extensions.validator.core.validation.strategy.DefaultValidationStrategyFactory;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import javax.faces.component.UIComponent;
import javax.faces.render.RenderKit;
import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Gerhard Petracek
 */
@UsageInformation(UsageCategory.INTERNAL)
public class FactoryUtils
{
    private static AnnotationExtractorFactory annotationExtractorFactory;

    @ToDo(value = Priority.MEDIUM, description = "logging")
    public static AnnotationExtractorFactory getComponentAnnotationExtractorFactory()
    {
        if (annotationExtractorFactory == null)
        {
            List<String> annotationExtractorFactoryClassNames = new ArrayList<String>();

            annotationExtractorFactoryClassNames
                .add(WebXmlParameter.CUSTOM_COMPONENT_ANNOTATION_EXTRACTOR_FACTORY);
            annotationExtractorFactoryClassNames.add(ExtValUtils
                .getInformationProviderBean()
                .getCustomComponentAnnotationExtractorFactory());
            annotationExtractorFactoryClassNames
                .add(DefaultComponentAnnotationExtractorFactory.class.getName());

            for (String className : annotationExtractorFactoryClassNames)
            {
                annotationExtractorFactory = (AnnotationExtractorFactory) ClassUtils
                    .tryToInstantiateClassForName(className);

                if (annotationExtractorFactory != null)
                {
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
                    break;
                }
            }
        }

        return messageResolverFactory;
    }

    private static ClassMappingFactory<ValidationStrategy, MetaDataExtractor> metaDataExtractorFactory;

    public static ClassMappingFactory<ValidationStrategy, MetaDataExtractor> getMetaDataExtractorFactory()
    {
        if (metaDataExtractorFactory == null)
        {
            List<String> metaDataExtractorFactoryClassNames = new ArrayList<String>();

            metaDataExtractorFactoryClassNames.add(WebXmlParameter.CUSTOM_META_DATA_EXTRACTOR_FACTORY);
            metaDataExtractorFactoryClassNames.add(ExtValUtils
                .getInformationProviderBean().getCustomMetaDataExtractorFactory());
            metaDataExtractorFactoryClassNames.add(DefaultMetaDataExtractorFactory.class.getName());

            for (String className : metaDataExtractorFactoryClassNames)
            {
                metaDataExtractorFactory = (ClassMappingFactory<ValidationStrategy, MetaDataExtractor>) ClassUtils
                    .tryToInstantiateClassForName(className);

                if (metaDataExtractorFactory != null)
                {
                    break;
                }
            }
        }

        return metaDataExtractorFactory;
    }

    private static ClassMappingFactory<RenderKit, RenderingContextInitializer> renderingContextInitializerFactory;

    public static ClassMappingFactory<RenderKit, RenderingContextInitializer> getRenderingContextInitializerFactory()
    {
        if (renderingContextInitializerFactory == null)
        {
            List<String> renderingContextInitializerFactoryClassNames = new ArrayList<String>();

            renderingContextInitializerFactoryClassNames
                .add(WebXmlParameter.CUSTOM_RENDERING_CONTEXT_INITIALIZER_FACTORY);
            renderingContextInitializerFactoryClassNames.add(ExtValUtils
                .getInformationProviderBean()
                .getCustomRenderingContextInitializerFactory());
            renderingContextInitializerFactoryClassNames
                .add(DefaultRenderingContextInitializerFactory.class.getName());

            for (String className : renderingContextInitializerFactoryClassNames)
            {
                renderingContextInitializerFactory =
                    (ClassMappingFactory<RenderKit, RenderingContextInitializer>) ClassUtils
                        .tryToInstantiateClassForName(className);

                if (renderingContextInitializerFactory != null)
                {
                    break;
                }
            }
        }

        return renderingContextInitializerFactory;
    }

    private static ClassMappingFactory<UIComponent, ComponentInitializer> componentInitializerFactory;

    public static ClassMappingFactory<UIComponent, ComponentInitializer> getComponentInitializerFactory()
    {
        if (componentInitializerFactory == null)
        {
            List<String> componentInitializerFactoryClassNames = new ArrayList<String>();

            componentInitializerFactoryClassNames
                .add(WebXmlParameter.CUSTOM_COMPONENT_INITIALIZER_FACTORY);
            componentInitializerFactoryClassNames.add(ExtValUtils
                .getInformationProviderBean()
                .getCustomComponentInitializerFactory());
            componentInitializerFactoryClassNames
                .add(DefaultComponentInitializerFactory.class.getName());

            for (String className : componentInitializerFactoryClassNames)
            {
                componentInitializerFactory = (ClassMappingFactory<UIComponent, ComponentInitializer>) ClassUtils
                    .tryToInstantiateClassForName(className);

                if (componentInitializerFactory != null)
                {
                    break;
                }
            }
        }

        return componentInitializerFactory;
    }
}
